

# Just anOther RELease assembler for Erlang/OTP Releases #

Copyright (c) 2015 Grégoire Lejeune, 2015-2016 G-Corp

__Version:__ 0.1.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)), Gregoire Lejeune ([`greg@g-corp.io`](mailto:greg@g-corp.io)).

![Jorel](https://raw.githubusercontent.com/emedia-project/jorel/master/Jor-El.jpeg)


## Install ##

Download [jorel](https://github.com/emedia-project/jorel/wiki/jorel), make it executable and place it in your PATH.


## Configuration ##
* `output_dir` : Output directory (default: `_jorel`)
* `exclude_dirs` : Path to exclude (default: `_jorel`)
* `include_src` : Include sources in the release (default: false)
* `sys_config` : Path to the configuration file (default: none)
* `vm_args` : Path to the `vm.args` file to use (default: none)
* `generate_start_script` : Generate start script (default: true)
* `include_erts` : Include ERTS in the release (default: true)
* `lib_dirs` : List of directory to search apps (default: none) / TODO
* `providers` : Add providers (default: none)
* `boot` : List of apps in boot script (default: all)
* `disable_relup` : Enable/disable relup (default: true)
* `env` : Environment (default: prod)
* `erts` : ERTS to use (default: local)



## Providers ##
* `jorel_provider_tar` : Create a Tar archive
* `jorel_provider_zip` : Create a Zip archive
* `jorel_provider_artifactory` : Deploy to artifactory. This provider use the following parameters : * `url` :: `string()` : Artifactory URL (mandatory).
* `username` :: `env | string()` : Username (optional).
* `password` :: `env | string()` : Password (optional).
* `repository` :: `string()` : Repository (mandatory).
* `checksum` :: `string()` : Checksum when you want to deploy with checksum (optional).
* `jorel_provider_deb` : Create a Debian package (you need to install `debuild` from `devscripts`). This provider use the following parameters :* `author_name`
* `author_email`
* `install_user`
* `install_user_desc`
* `package_url`
* `package_git`
* `package_shortdesc`
* `package_desc`
* `package_depends`
* `license_type`
* `copyright`
* `jorel_provider_git_tag` : Create a git tag



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
<tr><td><a href="https://github.com/emedia-project/jorel/blob/master/doc/jorel.md" class="module">jorel</a></td></tr></table>

