# Generated by stackage2nix 0.7.1 from "/home/dbushev/projects/commercialhaskell/lts-haskell/lts-12.2.yaml"
{ pkgs, haskellLib }:

with haskellLib; self: super: {

  # core packages
  "array" = null;
  "base" = null;
  "binary" = null;
  "bytestring" = null;
  "containers" = null;
  "deepseq" = null;
  "directory" = null;
  "filepath" = null;
  "ghc-boot" = null;
  "ghc-boot-th" = null;
  "ghc-prim" = null;
  "ghci" = null;
  "hpc" = null;
  "integer-gmp" = null;
  "pretty" = null;
  "process" = null;
  "rts" = null;
  "template-haskell" = null;
  "terminfo" = null;
  "time" = null;
  "transformers" = null;
  "unix" = null;
  # break cycle: HUnit call-stack nanospec hspec hspec-core clock tasty async hashable test-framework-hunit tasty-quickcheck tasty-hunit hspec-expectations hspec-meta quickcheck-io silently temporary exceptions hspec-discover stringbuilder
  "stringbuilder" = dontCheck super.stringbuilder;
  "hspec-discover" = dontCheck super.hspec-discover;
  "exceptions" = dontCheck super.exceptions;
  "temporary" = dontCheck super.temporary;
  "silently" = dontCheck super.silently;
  "hashable" = dontCheck super.hashable;
  "async" = dontCheck super.async;
  "clock" = dontCheck super.clock;
  "nanospec" = dontCheck super.nanospec;

}