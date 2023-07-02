To build this project just run `nix build`. To hack it run `nix develop` to bring in all dependencies.

Feel free to raise PRs. Small iterations are always better than major rewrites.

If you are not confident writing Haskell you may find it easier to add a test case for what you want in `test/Spec.hs`.
This is easy to do without really understanding the code by copying a test case and editing the parameters ands expected result and is probably the easiest way to communicate an intended change.
Just creating an issue is ok too.
