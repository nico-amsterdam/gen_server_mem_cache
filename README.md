# GenServerMemCache

Trade memory for performance

In-memory key-value cache with expiration-time after creation/modification/access, automatic value loading and time travel support.

## Before you start

- If you want apply this for your database, consider to try this first:
  - Do everything you possibly can to lower the network latency between application servers and database
  - Use the database cache. Databases are optimized for caching and it has the great advantage that the database knows when it should update a cached item.

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

- time is in minutes
- Note for automatic reloading: think about what happens

scraped news

### Keep in cache for a limited time but extend life-time everytime it is accessed

list of countries

### Keep as long this process is running

file load product specifications

- schedule regular updates, using put

### Invalidate cached item

remove

## iex demo

```sh
$ iex -S mix

iex(1)> pid = GenServerMemCache.start_link()
```
