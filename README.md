# xrel

eXtanded Release assembler for Erlang/OTP Releases

## Install

Download [xrel](https://github.com/emedia-project/xrel/wiki/xrel), make it executable and place it in your PATH.

## Configuration

* `output_dir` : Output directory (default: `_xrel`)
* `exclude_dirs` : Path to exclude (default: `_xrel`)
* `include_src` : Include sources in the release (default: false)
* `sys_config` : Path to the configuration file (default: none)
* `vm_args` : Path to the `vm.args` file to use (default: none)
* `generate_start_script` : Generate start script (default: true)
* `include_erts` : Include ERTS in the release (default: true)
* `lib_dirs` : List of directory to search apps (default: none) / TODO
* `providers` : Add providers (default: none)
* `boot` : List of apps in boot script (default: all)
* `disable_relup` : Enable/disable relup (default: true)

## Providers

* `xrel_provider_tar` : Create a Tar archive
* `xrel_provider_zip` : Create a Zip archive
* `xrel_provider_artifactory` : Deploy to artifactory. This provider use the following parameters :
  * `url` :: `string()` : Artifactory URL (mandatory).
  * `username` :: `env | string()` : Username (optional).
  * `password` :: `env | string()` : Password (optional).
  * `repository` :: `string()` : Repository (mandatory).
  * `checksum` :: `string()` : Checksum when you want to deploy with checksum (optional).
* `xrel_provider_deb` : Create a Debian package (you need to install `debuild` from `devscripts`). This provider use the following parameters :
  * `author_name`
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

## Contributing

1. Fork it ( https://github.com/emedia-project/xrel/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

