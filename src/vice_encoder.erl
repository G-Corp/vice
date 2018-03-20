% @version 0.0.1
%
% @doc
% Generic encoder behavior
%
% <h3>Create an encoder</h3>
%
% Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam et felis
% ornare, scelerisque velit vitae, pretium ante. Class aptent taciti sociosqu
% ad litora torquent per conubia nostra, per inceptos himenaeos.
%
% To create a custom encoder, juste create a new module followinf this behavior.
%
% <h4><tt>init/0</tt></h4>
%
% The <tt>init/0</tt> function is called to initialize the encoder. This function
% must return a tuple :
%
% <ul>
% <li><tt>{ok, State :: term()}</tt> : the <tt>State</tt> will be passed to all other
% required functions of the module.</li>
% <li><tt>{stop, Reason :: term()}</tt> : this tuple is returned if the initialisation
% failed.</li>
% </ul>
%
% To help you create the initializer, VICE offer you the <tt>vice_utils:find_tools/2</tt>
% function. This function take a liste of executables (as a list of atoms) and an atom,
% as parameters.
%
% In case of success, this function will return a tuple
% <tt>{state, List :: list(tuple())}</tt>. In the list of tuple, the first element of each
% tuple is the input executable (atom) and the second element is the string to the
% executable path.
%
% Example :
%
% <pre>
% vice_utils:find_tools([ffmpeg, ffprobe], video).
% % => {state, [{ffmpeg, "/usr/bin/ffmpeg"}, {ffprobe, "/usr/bin/ffprobe"}]}
% </pre>
%
% The second parameter for <tt>vice_utils:find_tools/2</tt> allow you to add
% an alternative configurtion. In the above example, if VICE can't find
% <i>ffprobe</i> for example, using this second parameter, it will this to find an
% alternative path in the configuration. For example, if you put something like that
% in your <tt>.config</tt> file :
%
% <pre>
% {video, [
%   {ffprobe, "/opt/ffmpeg/bin/ffprobe"}
% ]}
% </pre>
%
% So, <tt>vice_utils:find_tools([ffmpeg, ffprobe], video)</tt> will return
% <tt>{state, [{ffmpeg, "/usr/bin/ffmpeg"}, {ffprobe, "/opt/ffmpeg/bin/ffprobe"}]}</tt>.
%
% If <tt>vice_utils:find_tools/2</tt> can't find (at least) one executable, it will return
% an error : <tt>{error, {App :: atom(), not_found}}</tt>.
%
% <blockquote>
% For now, VICE only support encoder using <i>command line</i> tools.
% </blockquote>
%
% <h4>infos/3</h4>
%
% <h4>info/4</h4>
%
% <h4>command/5</h4>
%
% <h4>progress/2</h4>
%
% <h3>Use you own encoder</h3>
%
% To determine which encoder to use, VICE use the type (from mimetype) of the
% input file.
%
% The encoders are declared in the <tt>vice</tt>/<tt>encoders</tt> section of
% the configuration. The default configration is the following :
%
% <pre>
% {vice, [
%   ...
%   {encoders, [
%     {video, [vice_prv_ffmpeg]},
%     {image, [vice_prv_imagemagick]},
%     {audio, [vice_prv_sox]}
%   ]},
%   ...
% }
% </pre>
%
% So if you create a new <tt>audio</tt> encoder, you must add it to the
% <tt>audio</tt> list :
%
% <pre>
% {vice, [
%   ...
%   {encoders, [
%     {audio, [my_custom_audio_encoder, vice_prv_sox]}
%   ]},
%   ...
% }
% </pre>
%
% In such case, VICE will try to load <tt>my_custom_audio_encoder</tt>.
% If <tt>my_custom_audio_encoder:init/0</tt> return <tt>{ok, State :: term()}</tt>
% it will not try to load <tt>vice_prv_sox</tt>. So <tt>my_custom_audio_encoder</tt>
% will be used for all audio encoding. If <tt>my_custom_audio_encoder:init/0</tt>
% return <tt>{error, Reason :: term()}</tt> VICE will try to load
% <tt>vice_prv_sox</tt> and if it succed, <tt>vice_prv_sox</tt> will be used for
% all audio encoding ; if not, VICE will not allow you to encode audio files.
%
% <blockquote>
% When you change the configuration, if you do not specified a list of encoders for
% a type, VICE will use its default list for this type.
% </blockquote>
% @end
-module(vice_encoder).

-type sofar() :: {Duration :: float(), Time :: float(), Percent :: float()}.

-callback init() ->
  {ok, State :: term()} | {stop, Reason :: term()}.

-callback infos(State :: term(),
                File :: file:filename_all(),
                Options :: list()) ->
  {ok, Result :: map()} | {error, Reason :: term()}.

-callback info(State :: term(),
               File :: file:filename_all(),
               Info :: atom(),
               Options :: list()) ->
  {ok, Result :: term()} | {error, Reason :: term()}.

-callback command(State :: term(),
                  In :: file:filename_all(),
                  Out :: file:filename_all(),
                  Options :: list(),
                  Multi :: true | false) ->
  {ok, Command :: string()} | {error, Reason :: term()}.

-callback progress(Bytes :: string(), Sofar :: sofar()) -> Sofar0 :: sofar().
