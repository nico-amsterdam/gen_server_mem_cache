# GenServerMemCache

Trade memory for performance

In-memory key-value cache with expiration-time after creation/modification/access (a.k.a. entry time-to-live and idle-timeout), automatic value loading and time travel support.

## Installation

  1. Add `gen_server_mem_cache` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:gen_server_mem_cache, github: "nico-amsterdam/gen_server_mem_cache"}]
    end
    ```

  2. mix deps.get
   
    ```sh
    $ mix deps.get
    ```
  
  3. Make `gen_server_mem_cache` part of your supervision tree:

    ```elixir
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Start the endpoint when the application starts (Phoenixframework)
      supervisor(MyProject.Endpoint, []),
      # Here you could define other workers and supervisors as children
      # worker(MyProject.Worker, [arg1, arg2, arg3]),
      worker(GenServerMemCache, [GenServerMemCache])
    ]
    ```

## Usage

### Keep in cache for a limited time, automatically load new value after that

  - Example: scrape news and keep it 10 minutes in cache

    ```elixir
    us_news = GenServerMemCache.cache(GenServerMemCache, "news_us", 10, &Scraper.scrape_news_us/0)
    ```

  - or with anonymous function:

      ```elixir
      def news(country) do
        GenServerMemCache.cache(GenServerMemCache, "news_" <> country, 10, fn -> Scraper.scrape_news(country) end)
      end
      ```

Note about automatically new value loading:
- How long this function take to get the new value, and is this acceptable when the old value is expired? If it takes too long, consider to use an scheduler to regularly recalculate the new value and update the cache with that.


### Keep in cache for a limited time but extend life-time everytime it is accessed

  - Example: cache http response of countries rest service for at least 20 minutes 

    ```elixir
    countries_response = GenServerMemCache.cache(GenServerMemCache, "countries_response", 20, true, fn -> HTTPoison.get! "http://restcountries.eu/rest/v1/" end)
    ```

### Keep as long this process is running

  - Example: cache products retrieved from csv file
    ```elixir
    products = GenServerMemCache.cache(GenServerMemCache, "products", fn -> "products.csv" |> File.stream! |> CSV.decode |> Enum.to_list  end)
    ```
    
  - updates are still possible:

    ```elixir
    GenServerMemCache.put(GenServerMemCache, "products", new_value)
    ```

or you can force an automatically load at first access by invalidating the cached item.

### Invalidate cached item

  - Example: remove products from cache

    ```elixir
    :ok = GenServerMemCache.remove(GenServerMemCache, "products")
    ```

## IEx demo

```sh
$ iex -S mix

iex(1)> {:ok, pid} = GenServerMemCache.start_link()                                                                         [7/1967]
{:ok, #PID<0.163.0>}
iex(2)> GenServerMemCache.get_all_entries(pid)
%{}
iex(3)> GenServerMemCache.put(pid, "key1", 1, "value1")
"value1"
iex(4)> IO.inspect :calendar.universal_time(); GenServerMemCache.get(pid, "key1")
{{2016, 7, 31}, {13, 7, 5}}
{:ok, "value1"}
iex(5)> GenServerMemCache.put(pid, "key2", %{"a" => 1, "b" => {1, 2, "whatever"}})
%{"a" => 1, "b" => {1, 2, "whatever"}}
iex(6)> GenServerMemCache.get!(pid, "key2") |> Map.get("b")
{1, 2, "whatever"}
iex(7)> IO.inspect :calendar.universal_time(); GenServerMemCache.get(pid, "key1")
{{2016, 7, 31}, {13, 7, 57}}
{:ok, "value1"}
iex(8)> IO.inspect :calendar.universal_time(); GenServerMemCache.get(pid, "key1")
{{2016, 7, 31}, {13, 8, 26}}
{:expired, "value1"}
iex(9)> GenServerMemCache.put(pid, "key3", "value3")
"value3"
iex(10)> GenServerMemCache.get_all_entries(pid)
%{"key1" => {"value1", 1469970477},
  "key2" => {%{"a" => 1, "b" => {1, 2, "whatever"}}, nil},
  "key3" => {"value3", nil}}
iex(11)> IO.inspect :calendar.universal_time(); GenServerMemCache.get(pid, "key1")
{{2016, 7, 31}, {13, 9, 6}}
{:expired, "value1"}
iex(12)> IO.inspect :calendar.universal_time(); GenServerMemCache.get(pid, "key1")
{{2016, 7, 31}, {13, 9, 21}}
{:not_cached, nil}
iex(13)> GenServerMemCache.get_all_entries(pid)
%{"key2" => {%{"a" => 1, "b" => {1, 2, "whatever"}}, nil},
  "key3" => {"value3", nil}}
iex(14)> GenServerMemCache.remove(pid, "key3")
:ok
iex(15)> GenServerMemCache.get(pid, "key3")
{:not_cached, nil}
iex(16)> GenServerMemCache.stop(pid)
:ok
```
