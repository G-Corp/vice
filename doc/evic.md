

# Module evic #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-2">convert/2</a></td><td>Equivalent to <a href="#convert-4"><tt>convert(In, Out, [], undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>
Return the given media informations.</td></tr><tr><td valign="top"><a href="#infos-1">infos/1</a></td><td>
Return the media informations.</td></tr><tr><td valign="top"><a href="#screenshot-2">screenshot/2</a></td><td>
Create a screenshot for a movie.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td>
Return the conversion status.</td></tr><tr><td valign="top"><a href="#to.md5_mp4-2">to_html5_mp4/2</a></td><td>Equivalent to <a href="#to.md5_mp4-3"><tt>to_html5_mp4(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_mp4-3">to_html5_mp4/3</a></td><td>
Convert the given movie for mp5 html5.</td></tr><tr><td valign="top"><a href="#to.md5_ogg-2">to_html5_ogg/2</a></td><td>Equivalent to <a href="#to.md5_ogg-3"><tt>to_html5_ogg(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_ogg-3">to_html5_ogg/3</a></td><td>
Convert the given movie for ogg html5.</td></tr><tr><td valign="top"><a href="#to.md5_webm-2">to_html5_webm/2</a></td><td>Equivalent to <a href="#to.md5_webm-3"><tt>to_html5_webm(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_webm-3">to_html5_webm/3</a></td><td>
Convert the given movie for webm html5.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>
Return the file type (image or video).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert-2"></a>

### convert/2 ###

`convert(In, Out) -> any()`

Equivalent to [`convert(In, Out, [], undefined)`](#convert-4).

<a name="convert-3"></a>

### convert/3 ###

`convert(In, Out, Options) -> any()`

Convert a media

<a name="convert-4"></a>

### convert/4 ###

`convert(In, Out, Options, Fun) -> any()`

Convert a media

<a name="info-2"></a>

### info/2 ###

`info(File, Info) -> any()`

Return the given media informations

<a name="infos-1"></a>

### infos/1 ###

`infos(File) -> any()`

Return the media informations

<a name="screenshot-2"></a>

### screenshot/2 ###

`screenshot(Movie, Out) -> any()`

Create a screenshot for a movie

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="status-1"></a>

### status/1 ###

`status(Worker) -> any()`

Return the conversion status

<a name="to_html5_mp4-2"></a>

### to_html5_mp4/2 ###

`to_html5_mp4(Input, Output) -> any()`

Equivalent to [`to_html5_mp4(Input, Output, undefined)`](#to.md5_mp4-3).

<a name="to_html5_mp4-3"></a>

### to_html5_mp4/3 ###

`to_html5_mp4(Input, Output, Fun) -> any()`

Convert the given movie for mp5 html5

<a name="to_html5_ogg-2"></a>

### to_html5_ogg/2 ###

`to_html5_ogg(Input, Output) -> any()`

Equivalent to [`to_html5_ogg(Input, Output, undefined)`](#to.md5_ogg-3).

<a name="to_html5_ogg-3"></a>

### to_html5_ogg/3 ###

`to_html5_ogg(Input, Output, Fun) -> any()`

Convert the given movie for ogg html5

<a name="to_html5_webm-2"></a>

### to_html5_webm/2 ###

`to_html5_webm(Input, Output) -> any()`

Equivalent to [`to_html5_webm(Input, Output, undefined)`](#to.md5_webm-3).

<a name="to_html5_webm-3"></a>

### to_html5_webm/3 ###

`to_html5_webm(Input, Output, Fun) -> any()`

Convert the given movie for webm html5

<a name="type-1"></a>

### type/1 ###

`type(File) -> any()`

Return the file type (image or video)

