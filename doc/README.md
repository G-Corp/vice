

# Just anOther RELease assembler for Erlang/OTP Releases #

Copyright (c) 2015 Grégoire Lejeune, 2015-2016 G-Corp

__Version:__ 0.2.0

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)), Gregoire Lejeune ([`greg@g-corp.io`](mailto:greg@g-corp.io)).

![Jorel](https://raw.githubusercontent.com/emedia-project/jorel/master/Jor-El.jpeg)


## Documentation ##

See [jorel.in](http://jorel.in) for documentation :

* [Command line tool](http://jorel.in/installation/#command-line)

* [Use with erlang.mk](http://jorel.in/installation/#erlangmk-plugin)

* [Use with rebar3](http://jorel.in/installation/#rebar3-plugin)

* [Use with mix](http://jorel.in/installation/#mix-task)



## Contributing ##
1. Fork it ( https://github.com/emedia-project/jorel/fork )
1. Create your feature branch (`git checkout -b my-new-feature`)
1. Commit your changes (`git commit -am 'Add some feature'`)
1. Push to the branch (`git push origin my-new-feature`)
1. Create a new Pull Request



## Using erlang.mk plugins ##

To use the [erlang.mk](http://erlang.mk/) plugins, add the following lines to your `Makefile` :

```

DEP_PLUGINS = jorel
REL_DEPS = jorel
dep_jorel = git https://github.com/emedia-project/jorel.git master

```

This will download and use the last binary released of Jorel.

If you prefer to compile Jorel, add the following lines to your `Makefile` :

```

DEP_PLUGINS = jorel
REL_DEPS = jorel
JOREL_BUILD = true
dep_jorel = git https://github.com/emedia-project/jorel.git master

```

Then you can run `make jorel.release` to create a release with Jorel.

See `make help` for more options.


## Licence ##

Copyright (c) 2015, Gregoire Lejeune<br />
Copyright (c) 2015-2016, G-Corp<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="jorel.md" class="module">jorel</a></td></tr></table>

