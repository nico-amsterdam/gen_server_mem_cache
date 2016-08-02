defmodule GenServerMemCacheTest do
  use ExUnit.Case, async: true

  defp generate_function_with_value(x) do
    fn() -> x end
  end

  # generate time function with a fixed time.
  defp get_time_travel_function(now, plus_minutes \\ 0, plus_seconds \\ 0) do
    generate_function_with_value(now + plus_minutes * 60 + plus_seconds)
  end

  test "value is expired" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_10 = get_time_travel_function(now, 10)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:expired, "value1"}, {_f_system_time, nil, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_10, nil, map2}) 

    assert map2 == map3
  end

  test "value is expired and removed" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_10 = get_time_travel_function(now, 10)
    f_now_plus_15 = get_time_travel_function(now, 15)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:noreply, {_f_system_time, nil, map3}} = 
      GenServerMemCache.handle_cast({:remove_expired_entries}, {f_now_plus_10, now, map2}) 

    assert map3 == %{}

    {:reply, {:not_cached, nil}, {_f_system_time, nil, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_15, nil, map3}) 

    assert map4 == map3
  end

  test "accessed before and after expired" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_5  = get_time_travel_function(now, 5 )
    f_now_plus_10 = get_time_travel_function(now, 10)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:ok, "value1"}, {_f_system_time, nil, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_5, nil, map2}) 

    assert map3 == map2

    {:reply, {:expired, "value1"}, {_f_system_time, nil, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_10, nil, map3}) 

    assert map4 == map3
  end

  test "accessed with keep alive" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_5  = get_time_travel_function(now, 5 )
    f_now_plus_10 = get_time_travel_function(now, 10)
    f_now_plus_20 = get_time_travel_function(now, 20)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:ok, "value1"}, {_f_system_time, _, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_5, nil, map2}) 

    assert map3 != map2
    assert map3 == %{"key1" => {"value1", now + 5 * 60 + 8 * 60 + 30}}

    {:reply, {:ok, "value1"}, {_f_system_time, _, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_10, nil, map3}) 

    assert map4 != map3
    assert map4 == %{"key1" => {"value1", now + 10 * 60 + 8 * 60 + 30}}

    # this might supprise you, but remove_expired_entries didn't run, so the expired key is awakened
    {:reply, {:ok, "value1"}, {_f_system_time, _, map5}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_20, nil, map4}) 

    assert map5 != map4
    assert map5 == %{"key1" => {"value1", now + 20 * 60 + 8 * 60 + 30}}
  end

  test "accessed with keep alive. remove has run" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_5  = get_time_travel_function(now, 5 )
    f_now_plus_10 = get_time_travel_function(now, 10)
    f_now_plus_20 = get_time_travel_function(now, 20)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:ok, "value1"}, {_f_system_time, _, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_5, nil, map2}) 

    assert map3 != map2
    assert map3 == %{"key1" => {"value1", now + 5 * 60 + 8 * 60 + 30}}

    # nothing has expired yet
    {:noreply, {_f_system_time, next_check_time, map4}} =
      GenServerMemCache.handle_cast({:remove_expired_entries}, {f_now_plus_10, now, map3}) 

    assert map4 == map3
    assert next_check_time == now + 10 * 60 + 60

    {:reply, {:ok, "value1"}, {_f_system_time, _, map5}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_10, nil, map4}) 

    assert map5 != map4
    assert map5 == %{"key1" => {"value1", now + 10 * 60 + 8 * 60 + 30}}

    {:noreply, {_f_system_time, nil, map6}} =
      GenServerMemCache.handle_cast({:remove_expired_entries}, {f_now_plus_20, now, map5}) 

    assert map6 != map5
    assert map6 == %{}

    {:reply, {:not_cached, nil}, {_f_system_time, _, map7}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_20, nil, map6}) 

    assert map7 == map6
  end

  test "no expire time" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_10 = get_time_travel_function(now, 10)
    f_now_plus_20 = get_time_travel_function(now, 20)
    f_now_plus_30 = get_time_travel_function(now, 30)
    map1 = %{}

    {:reply, :ok, {_f_system_time, nil, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", nil}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", nil}}

    {:noreply, {_f_system_time, nil, map3}} = 
      GenServerMemCache.handle_cast({:remove_expired_entries}, {f_now_plus_10, now, map2}) 

    assert map3 == map2

    {:reply, {:ok, "value1"}, {_f_system_time, nil, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_20, nil, map3}) 

    assert map4 == map3

    {:reply, :ok, {_f_system_time, nil, map5}} = 
      GenServerMemCache.handle_call({:put, "key1", "value2", nil}, pid, {f_now_plus_30, nil, map4})

    assert map5 == %{"key1" => {"value2", nil}}
  end

  test "first no expire time" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_10 = get_time_travel_function(now, 10)
    f_now_plus_20 = get_time_travel_function(now, 20)
    f_now_plus_30 = get_time_travel_function(now, 30)
    map1 = %{}

    {:reply, :ok, {_f_system_time, nil, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", nil}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", nil}}

    {:reply, {:ok, "value1"}, {_f_system_time, _, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_10, nil, map2}) 

    assert map3 != map2
    assert map3 == %{"key1" => {"value1", now + 10 * 60 + 8 * 60 + 30}}

    {:reply, {:expired, "value1"}, {_f_system_time, nil, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_20, nil, map3}) 

    assert map4 == map3

    # a new put overwrites whatever expire time there was
    {:reply, :ok, {_f_system_time, nil, map5}} = 
      GenServerMemCache.handle_call({:put, "key1", "value2", nil}, pid, {f_now_plus_30, nil, map4})

    assert map5 == %{"key1" => {"value2", nil}}
  end

  test "keep alive extension within a minute" do
    # This test the optimalization that we don't want to change the map each second. Once a minute is ok.
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_5  = get_time_travel_function(now, 5 )
    f_now_plus_5_plus_45_seconds = get_time_travel_function(now, 5, 45)
    f_now_plus_6 = get_time_travel_function(now, 6)
    map1 = %{}

    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:ok, "value1"}, {_f_system_time, _, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_5, nil, map2}) 

    assert map3 != map2
    assert map3 == %{"key1" => {"value1", now + 5 * 60 + 8 * 60 + 30}}

    # this does not set a new expire time, it has just been moved
    {:reply, {:ok, "value1"}, {_f_system_time, _, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_5_plus_45_seconds, nil, map3}) 

    assert map4 == map3

    # it's a minute later, so now we add 1 minute to the expire time
    {:reply, {:ok, "value1"}, {_f_system_time, _, map5}} = 
      GenServerMemCache.handle_call({:get, "key1", 8, false}, pid, {f_now_plus_6, nil, map4}) 

    assert map5 != map4
    assert map5 == %{"key1" => {"value1", now + 6 * 60 + 8 * 60 + 30}}

  end

  test "expire warning" do
    pid = :erlang.self()
    now = :os.system_time(:seconds)
    f_now = get_time_travel_function(now)
    f_now_plus_7_plus_30_seconds = get_time_travel_function(now, 7, 30)
    f_now_plus_8_plus_31_second = get_time_travel_function(now, 8, 31)
    map1 = %{}

    # in this case we only have an initial expire time and we don't keep it alive
    {:reply, :ok, {_f_system_time, next_check_time, map2}} = 
      GenServerMemCache.handle_call({:put, "key1", "value1", 8}, pid, {f_now, nil, map1})

    assert map2 == %{"key1" => {"value1", now + 8 * 60}}
    assert next_check_time == now + 60

    {:reply, {:expire_warning, "value1"}, {_f_system_time, nil, map3}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, true}, pid, {f_now_plus_7_plus_30_seconds, nil, map2}) 

    assert map3 != map2
    assert map3 == %{"key1" => {"value1", now + 7 * 60 + 30 + 60}}

    # we can also suppress this expire_warning for the client interface get-method. 
    # Only the client cache-method uses the warning, because that method garanties that it will try to set a new value.
    {:reply, {:ok, "value1"}, {_f_system_time, nil, map4}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_7_plus_30_seconds, nil, map3}) 

    assert map4 == map3

    # If we fail to set a new value, the result is that the expire_warning gave us extra time: 30 till 59 seconds.
    {:reply, {:expired, "value1"}, {_f_system_time, nil, map5}} = 
      GenServerMemCache.handle_call({:get, "key1", nil, false}, pid, {f_now_plus_8_plus_31_second, nil, map4}) 

    assert map5 == map4

  end
end
