# Shellify

The [`nix-shell`](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html) utility with the `-p` switch is great to quickly add dependencies to get something to build. It's easy to work quickly and iteratively. Once you're done you should save a `shell.nix` so that you and anybody else can quickly and instantly create an environment with the same dependencies. With this tool you can quickly change `nix-shell` to `nix-shellify` to create a `shell.nix`. This saves searches and typing. Now saving a basic working `shell.nix` is almost instant.

## Prerequisites

This utility assumes [nix is installed](https://nixos.org/download.html).


## Example usage

I want to run something called `foo`. When I run `./foo` it complains I don't have python. So I run `nix-shell -p python` and try running `./foo` again.

Now it complains I din't have asciidoc so I come out of the shell and run `nix-shell -p python asciidoc`. Now it works.

I exit my shell and change `nix-shell -p python asciidoc` to `nix-shellify -p python asciidoc`. It then creates a `shell.nix` which I keep next to foo. Now I can just type `nix-shell` and I have what I need to run foo. I can share and everybody else can run foo too without the same search.
