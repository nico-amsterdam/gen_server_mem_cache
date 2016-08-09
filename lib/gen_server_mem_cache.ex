defmodule GenServerMemCache do
  use GenServer

  @moduledoc ~S'''
  In-memory key-value cache with optional expiration-time after creation/modification/access,
  automatic value loading and time travel support.
  '''

  ## Client API

  @doc ~S'''
  Start the memory cache process linked to the current process.

  The `f_system_time` parameter is meant for time travel support. 
  You can provide a function that returns the Unix/Posix UTC time in seconds.
  Default: nil - use current system time.

  ## Example

    It's recommended to start the process in a supervision tree.
 
      def start(_type, _args) do
        import Supervisor.Spec, warn: false

        children = [
          # Start the endpoint when the application starts (Phoenixframework)
          supervisor(MyProject.Endpoint, []),
          # Here you could define other workers and supervisors as children
          # worker(MyProject.Worker, [arg1, arg2, arg3]),
          worker(GenServerMemCache, [GenServerMemCache])
        ]

        # See http://elixir-lang.org/doc<F3>s/stable/elixir/Supervisor.html
        # for other strategies and supported options
        opts = [strategy: :one_for_one, name: MyProject.Supervisor]
        Supervisor.start_link(children, opts)
      end
 
  '''
  @spec start_link(GenServer.name, (() -> integer)) :: GenServer.on_start
  def start_link(name \\ nil, f_system_time \\ nil) do
    GenServer.start_link(__MODULE__, f_system_time || &system_time/0, name: name)
  end

  @doc ~S'''
  Cache value returned by the supplied function.

  * `gen_server_name` - Name of the cache. See start_link or supervisor tree.
  * `key` - Name of the key
  * `minutes_valid` - Minutes to keep the item in cache. Default: nil - do not expire.
  * `keep_alive` - Keep the item in cache if it still accessed. It expires if it is not retrieved in `minutes_valid` minutes. Default: false
  * `f_new_value` - function that supplies the value. Enables automatic value loading. When `minutes_valid` is not nil and `keep_alive` is false, this function can be launched in a new Erlang process. 

  ## Examples

      products = GenServerMemCache.cache(GenServerMemCache, "products", 30, true, 
         fn() -> File.read!(filename) end
      )

      news_page = GenServerMemCache.cache(GenServerMemCache, "news", 10, &scrape_news/0)

  '''
  @spec cache(GenServer.name, Map.key, integer, boolean, (() -> any)) :: any
  def cache(gen_server_name, key, minutes_valid \\ nil, keep_alive \\ false, f_new_value) do
    cache_value = GenServer.call(gen_server_name, {:get, key, (if keep_alive, do: minutes_valid, else: nil), true})
    case cache_value do
       {:ok, value}             -> value
       {:expire_warning, value} -> spawn(fn -> put(gen_server_name, key, minutes_valid, f_new_value.()) end)
                                   value
       _                        -> put(gen_server_name, key, minutes_valid, f_new_value.())
    end
  end

  @doc "Create or update an item in cache. Returns item value."
  @spec put(GenServer.name, Map.key, integer, Map.value) :: any
  def put(gen_server_name, key, minutes_valid \\ nil, value) do
    GenServer.call(gen_server_name, {:put, key, minutes_valid, value})
  end

  @doc "Remove an item from cache."
  @spec remove(GenServer.name, Map.key) :: term
  def remove(gen_server_name, key) do
    GenServer.call(gen_server_name, {:remove, key})
  end

  @doc ~S'''
  Get status and value of cached item. Status can be :ok, :expired or :not_cached

  The `minutes_keep_alive` parameter is the number of minutes to keep the item at least in cache. 
  Does not shorten a previously set expiration time (use put for that). However, 
  if there wasn't an expiration time it will take the new value. Default: nil - do not change the expire time.

  ## Example
    
      iex(1)> products = GenServerMemCache.get(GenServerMemCache, "products", 20)
      {:expired, "Fret dots"}
    
  '''
  @spec get(GenServer.name, Map.key, integer) :: {term, any}
  def get(gen_server_name, key, minutes_keep_alive \\ nil) do
    GenServer.call(gen_server_name, {:get, key, minutes_keep_alive, false})
  end

  @doc "Get value of cached item. Nil if is not cached or when value is nil."
  @spec get!(GenServer.name, Map.key, integer) :: any
  def get!(gen_server_name, key, minutes_keep_alive \\ nil) do
    {_, value} = GenServer.call(gen_server_name, {:get, key, minutes_keep_alive, false})
    value
  end

  @doc "Get the complete cache key-value map. The value is a tupple: {value, expiration Unix/Posix time in seconds}."
  @spec get_all_entries(GenServer.name) :: map
  def get_all_entries(gen_server_name) do
    GenServer.call(gen_server_name, {:get_all_entries})
  end

  @doc "Remove expired entries. This is automatically called during GenServerMemCache usage."
  @spec remove_expired_entries(GenServer.name) :: :ok
  def remove_expired_entries(gen_server_name) do
    GenServer.cast(gen_server_name, {:remove_expired_entries})
  end

  @doc "Stops the cache process."
  @spec stop(GenServer.name) :: :ok
  def stop(gen_server_name) do
    GenServer.stop(gen_server_name)
  end

  # return Unix/Posix UTC time in seconds
  defp system_time do
    :os.system_time(:seconds)
  end

  ## Server Callbacks

  # GenServer calling itself:
  defp check_expired(now, expire_check_time) do
    if !is_nil(expire_check_time) and now >= expire_check_time do
       remove_expired_entries(:erlang.self())
    end
    :ok
  end

  @doc false
  def init(f_system_time) do
    initial_state = {f_system_time, nil, %{}}
    {:ok, initial_state}
  end

  @doc false
  def handle_call({:put, key, minutes_valid, value}, _from, {f_system_time, expire_check_time, map}) do
    now = f_system_time.()
    expires = if is_nil(minutes_valid), do: nil, else: now + minutes_valid * 60
    m = Map.put(map, key, {value, expires})
    next_check_time = if is_nil(expire_check_time) and minutes_valid != nil, do: f_system_time.() + 60, else: expire_check_time
    check_expired(now, expire_check_time)
    {:reply, value, {f_system_time, next_check_time, m}}
  end

  def handle_call({:remove, key}, _from, {f_system_time, expire_check_time, map}) do
    m = Map.delete(map, key)
    check_expired(f_system_time.(), expire_check_time)
    {:reply, :ok, {f_system_time, expire_check_time, m}}
  end

  def handle_call({:get, key, minutes_keep_alive, warn}, _from, {f_system_time, expire_check_time, map}) do
    map_value = Map.get(map, key)
    now = f_system_time.()
    {status, value, expires} = case map_value do
       nil -> {:not_cached, nil, nil}
       {value, expires} when minutes_keep_alive != nil and minutes_keep_alive >= 0 -> {:ok, value, expires}
       {value, expires} when is_nil(expires) and is_nil(minutes_keep_alive) -> {:ok, value, expires}
       {value, expires} when warn and is_nil(minutes_keep_alive) and expires <= now + 30 and expires > now -> {:expire_warning, value, expires}
       {value, expires} when expires != nil and expires >= now -> {:ok, value, expires}
       {old_value, expires} -> {:expired, old_value, expires}
    end
    m = cond do
       is_nil(map_value) -> map
       # The client interface cache-method handles expire_warnings, therefore:
       # 1 till 30 seconds before expiring, the first caller gets a warning and 30 seconds to come with a new value. 
       # During the next 30 seconds other clients receive ok, with the existing cached value.
       # After 30 seconds, if no new value is set, again an expire_warning will be given. 
       # The purpose of this: for very frequent requested keys, don't create a storm when the value expires.
       status == :expire_warning -> %{map | key => {value, now + 60}}
       is_nil(minutes_keep_alive) -> map
       is_nil(expires) or (now + minutes_keep_alive * 60 >= expires + 30) -> %{map | key => {value, now + minutes_keep_alive * 60 + 30}}
       true -> map
    end
    next_check_time = if is_nil(expire_check_time) and minutes_keep_alive != nil, do: f_system_time.() + 60, else: expire_check_time
    check_expired(now, expire_check_time)
    {:reply, {status, value}, {f_system_time, next_check_time, m}}
  end

  def handle_call({:get_all_entries}, _from, {f_system_time, expire_check_time, map}) do
    {:reply, map, {f_system_time, expire_check_time, map}}
  end

  @doc false
  def handle_cast({:remove_expired_entries}, {f_system_time, expire_check_time, map}) do
    now = f_system_time.()
    if !is_nil(expire_check_time) and now >= expire_check_time do
       m = map
           |> Enum.filter(fn {_key, {_value, expires}} -> is_nil(expires) or expires >= now end)
           |> Enum.into(%{})
       has_expire = Enum.any?(m, fn {_key, {_value, expires}} -> !is_nil(expires) end)
       case has_expire do
         false -> {:noreply, {f_system_time, nil, m}}
         true  -> {:noreply, {f_system_time, f_system_time.() + 60, m}}
       end
     else 
       {:noreply, {f_system_time, expire_check_time, map}}
     end
  end

end
