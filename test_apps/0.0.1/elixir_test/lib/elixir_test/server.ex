defmodule ElixirTest.Server do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def push(item) do
    GenServer.cast(__MODULE__, {:push, item})
  end

  def pop() do
    GenServer.call(__MODULE__, :pop)
  end

  def handle_call(:pop, _from, []) do
    {:reply, :empty, []}
  end
  def handle_call(:pop, _from, [h|t]) do
    {:reply, h, t}
  end

  def handle_call(request, from, state) do
    # Call the default implementation from GenServer
    super(request, from, state)
  end

  def handle_cast({:push, item}, state) do
    {:noreply, [item|state]}
  end

  def handle_cast(request, state) do
    super(request, state)
  end
end
