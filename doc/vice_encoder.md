

# Module vice_encoder #
* [Description](#description)


Generic encoder behavior.

__Version:__ 0.0.1

__This module defines the `vice_encoder` behaviour.__<br /> Required callback functions: `init/0`, `infos/3`, `info/4`, `command/5`, `progress/2`.

<a name="description"></a>

## Description ##


### Create an encoder ###

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam et felis 
ornare, scelerisque velit vitae, pretium ante. Class aptent taciti sociosqu 
ad litora torquent per conubia nostra, per inceptos himenaeos.

To create a custom encoder, juste create a new module followinf this behavior.


#### `init/0` ####

The `init/0` function is called to initialize the encoder. This function 
must return a tuple :

* `{ok, State :: term()}` : the `State` will be passed to all other
required functions of the module.

* `{stop, Reason :: term()}` : this tuple is returned if the initialisation
failed.


To help you create the initializer, VICE offer you the `vice_utils:find_tools/2` 
function. This function take a liste of executables (as a list of atoms) and an atom, 
as parameters.

In case of success, this function will return a tuple
`{state, List :: list(tuple())}`. In the list of tuple, the first element of each 
tuple is the input executable (atom) and the second element is the string to the 
executable path.

Example :

```

 vice_utils:find_tools([ffmpeg, ffprobe], video).
 % => {state, [{ffmpeg, "/usr/bin/ffmpeg"}, {ffprobe, "/usr/bin/ffprobe"}]}
```

The second parameter for `vice_utils:find_tools/2` allow you to add
an alternative configurtion. In the above example, if VICE can't find
_ffprobe_ for example, using this second parameter, it will this to find an
alternative path in the configuration. For example, if you put something like that
in your `.config` file :

```

 {video, [
   {ffprobe, "/opt/ffmpeg/bin/ffprobe"}
 ]}
```

So, `vice_utils:find_tools([ffmpeg, ffprobe], video)` will return
`{state, [{ffmpeg, "/usr/bin/ffmpeg"}, {ffprobe, "/opt/ffmpeg/bin/ffprobe"}]}`.

If `vice_utils:find_tools/2` can't find (at least) one executable, it will return
an error : `{error, {App :: atom(), not_found}}`.

<blockquote>
For now, VICE only support encoder using _command line_ tools.
</blockquote>


#### infos/3 ####


#### info/4 ####


#### command/5 ####


#### progress/2 ####


### Use you own encoder ###

To determine which encoder to use, VICE use the type (from mimetype) of the 
input file.

The encoders are declared in the `vice`/`encoders` section of 
the configuration. The default configration is the following :

```

 {vice, [
   ...
   {encoders, [
     {video, [vice_prv_ffmpeg]},
     {image, [vice_prv_imagemagick]},
     {audio, [vice_prv_sox]}
   ]},
   ...
 }
```

So if you create a new `audio` encoder, you must add it to the
`audio` list :

```

 {vice, [
   ...
   {encoders, [
     {audio, [my_custom_audio_encoder, vice_prv_sox]}
   ]},
   ...
 }
```

In such case, VICE will try to load `my_custom_audio_encoder`.
If `my_custom_audio_encoder:init/0` return `{ok, State :: term()}`
it will not try to load `vice_prv_sox`. So `my_custom_audio_encoder`
will be used for all audio encoding. If `my_custom_audio_encoder:init/0`
return `{error, Reason :: term()}` VICE will try to load
`vice_prv_sox` and if it succed, `vice_prv_sox` will be used for 
all audio encoding ; if not, VICE will not allow you to encode audio files.

<blockquote>
When you change the configuration, if you do not specified a list of encoders for
a type, VICE will use its default list for this type.
</blockquote>
