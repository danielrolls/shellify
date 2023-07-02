# Shellify

Want to quicky get something to build and to have it reproducable and sharable? The [`nix-shell`](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html) utility with the `-p` switch is great to quickly add dependencies to get something to build. It's easy to work quickly and iteratively. Once you're done you should save a `shell.nix` so that you and anybody else can quickly and instantly create an environment with the same dependencies. With this tool you can quickly change `nix-shell` to `nix-shellify` to create a `shell.nix`. This saves searches and typing. Now saving a basic working `shell.nix` is almost instant.

## Prerequisites

This utility assumes [nix is installed](https://nixos.org/download.html).


## Example usage without flakes

I want to run a prgram called `foo`. When I run `./foo` it complains I don't have python. So I run `nix-shell -p python` and try running `./foo` again.

Now it complains I didn't have asciidoc so I come out of the shell and edit the last command to add asciideoc by running `nix-shell -p python asciidoc`. Now it works.

I exit my shell and change `nix-shell -p python asciidoc` to `nix-shellify -p python asciidoc`. It then creates a `shell.nix` which I keep next to foo. Now I can just type `nix-shell` and I have what I need to run foo. I can share and everybody else can run foo too without the same search. They also don't need to run it in a conatainer or VM or to maintain depenencies on their system themself. 

## Example usage with flakes

I want to run something called `foo`. When I run `./foo` it complains I don't have python. So I run `nixshell nixpkgs#python` and try running `./foo` again.

Now it complains I didn't have asciidoc so I come out of the shell and edit the last command to add asciideoc by running `nix shell nixpkgs#python nixpkgs#asciidoc`. Now it works.

I exit my shell and change `nix shell nixpkgs#python nixpkgs#asciidoc` to `nix-shellify --with-flakes nixpkgs#python nixpkgs#asciidoc`. It then creates a `flake.nix` and `shell.nix` which I commit in git next to foo. Now I can just type `nix develop` and I have what I need to run foo. I can share and everybody else can run foo too without the same search. I can also go on to tell nix to run as an app or to just make `foo` avaiable handling dependencies for me. 

## Contributions

To build this project just run `nix build`. To hack it run `nix develop` to bring in all dependencies. Feel free to raise PRs. Small iterations are always better than major rewrites. If you are ont confident writing Haskell you may find it easier to add a test case for what you want in `test/Spec.hs`. This is easy to do without really understamnding the code by copying a test case and editing the parameters ands expected result. This is probably the easiest way to communicate an intended change. Just creating an issue is ok too.
