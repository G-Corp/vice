-module(jorel_sample).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, push/1, pop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

push(Data) ->
  gen_server:call(?SERVER, {push, Data}).

pop() ->
  gen_server:call(?SERVER, pop).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
  {ok, Args}.

handle_call({push, Data}, _From, State) ->
  {reply, ok, [Data|State]};
handle_call(pop, _From, [Data|State]) ->
  {reply, {ok, Data}, State};
handle_call(pop, _From, []) ->
  {reply, empty, []};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
