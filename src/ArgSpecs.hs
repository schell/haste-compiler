-- | All of hastec's command line arguments.
module ArgSpecs (argSpecs) where
import Args
import Haste.Config
import Haste.Environment
import Data.JSTarget.PP (debugPPOpts)
import Data.List (isSuffixOf)
import System.FilePath ((</>))

argSpecs :: [ArgSpec Config]
argSpecs = [
    ArgSpec { optName = "debug",
              updateCfg = \cfg _ -> cfg {ppOpts = debugPPOpts},
              info = "Output indented, fairly readable code, with all " ++
                     "external names included in comments."},
    ArgSpec { optName = "full-unicode",
              updateCfg = \cfg _ -> fullUnicode cfg,
              info = "Enable full Unicode support. Will bloat your output by "
                     ++ " 10-150 KB depending on usage and other options."},
    ArgSpec { optName = "dont-link",
              updateCfg = \cfg _ -> cfg {performLink = False},
              info = "Don't perform linking."},
    ArgSpec { optName = "libinstall",
              updateCfg = \cfg _ -> cfg {targetLibPath = jsmodDir,
                                         performLink   = False},
              info = "Install all compiled modules into the user's jsmod "
                     ++ "library rather than linking them together into a JS"
                     ++ "blob."},
    ArgSpec { optName = "opt-all",
              updateCfg = optAllSafe,
              info = "Enable all safe optimizations. "
                     ++ "Equivalent to -O2 --opt-google-closure --opt-whole-program."},
    ArgSpec { optName = "opt-all-unsafe",
              updateCfg = optAllUnsafe,
              info = "Enable all safe and unsafe optimizations. "
                     ++ "Equivalent to --opt-all --opt-unsafe-ints."},
    ArgSpec { optName = "opt-google-closure",
              updateCfg = updateClosureCfg,
              info = "Run the Google Closure compiler on the output. "
                   ++ "Use --opt-google-closure=foo.jar to hint that foo.jar "
                   ++ "is the Closure compiler."},
    ArgSpec { optName = "opt-google-closure-flag=",
              updateCfg = updateClosureFlags,
              info = "Add an extra flag for the Google Closure compiler to take. "
                   ++ "Use --opt-google-closure-flag='--language_in=ECMASCRIPT5_STRICT', "
                   ++ "to optimize programs to strict mode"},
    ArgSpec { optName = "opt-sloppy-tce",
              updateCfg = useSloppyTCE,
              info = "Allow the possibility that some tail recursion may not "
                     ++ "be optimized, to get slightly smaller code."},
    ArgSpec { optName = "opt-unsafe-ints",
              updateCfg = unsafeMath,
              info = "Enable all unsafe Int math optimizations. Equivalent to "
                     ++ "--opt-unsafe-mult --opt-vague-ints"},
    ArgSpec { optName = "opt-unsafe-mult",
              updateCfg = unsafeMul,
              info = "Use Javascript's built-in multiplication operator for "
                     ++ "fixed precision integer multiplication. This speeds "
                     ++ "up Int multiplication by a factor of at least four, "
                     ++ "but may give incorrect results when the product "
                     ++ "falls outside the interval [-2^52, 2^52]."},
    ArgSpec { optName = "opt-vague-ints",
              updateCfg = vagueInts,
              info = "Int math has 53 bits of precision, but gives incorrect "
                     ++ "results rather than properly wrapping around when "
                     ++ "those 53 bits are exceeded. Bitwise operations still "
                     ++ "only work on the lowest 32 bits. This option should "
                     ++ "give a substantial performance boost for Int math "
                     ++ "heavy code."},
    ArgSpec { optName = "opt-whole-program",
              updateCfg = enableWholeProgramOpts,
              info = "Perform optimizations over the whole program at link "
                     ++ "time. May significantly increase compilation time."},
    ArgSpec { optName = "out=",
              updateCfg = \cfg outfile -> cfg {outFile = \_ _ -> head outfile},
              info = "Write compiler output to <arg>."},
    ArgSpec { optName = "output-html",
              updateCfg = \cfg _ -> cfg {outputHTML = True},
              info = "Produce a skeleton HTML file containing the program."},
    ArgSpec { optName = "separate-namespace",
              updateCfg = \cfg _ -> cfg {wrapProg = True},
              info = "Wrap the program in its own namespace? "
                     ++ "Off by default since it may hurt performance."},
    ArgSpec { optName = "start=",
              updateCfg = \cfg str -> cfg {appStart = startCustom (head str)},
              info = "Specify how the Haste application will launch. Can be "
                     ++ "either asap, onload or a custom string "
                     ++ "containing the character sequence '%%', which will "
                     ++ "be replaced with the program's entry point function. "
                     ++ "The default is onload."
                     ++ " Note that '%%' will be replaced by the main function"
                     ++ " itself, not a call to it. Thus, in order to actually"
                     ++ " call the function, one would use '%%()'."},
    ArgSpec { optName = "trace-primops",
              updateCfg = \cfg _ -> cfg {tracePrimops = True,
                                         rtsLibs = debugLib : rtsLibs cfg},
              info = "Turn on run-time tracing of primops."},
    ArgSpec { optName = "verbose",
              updateCfg = \cfg _ -> cfg {verbose = True},
              info = "Display even the most obnoxious warnings."},
    ArgSpec { optName = "with-js=",
              updateCfg = \cfg args -> cfg {jsExternals = args},
              info = "Comma-separated list of .js files to include in the "
                   ++ "final JS bundle."}
  ]

-- | Compose two config update functions. Be careful about using any args with
--   them though!
(|||) :: (Config -> [String] -> Config)
     -> (Config -> [String] -> Config)
     -> (Config -> [String] -> Config)
a ||| b = \cfg args -> b (a cfg args) args

-- | Don't wrap Ints.
vagueInts :: Config -> [String] -> Config
vagueInts cfg _ = cfg {wrapIntMath = id}

-- | Use fast but unsafe multiplication.
unsafeMul :: Config -> [String] -> Config
unsafeMul cfg _ = cfg {multiplyIntOp = fastMultiply}

-- | Enable all unsafe math ops. Remember to update the info text when changing
--   this!
unsafeMath :: Config -> [String] -> Config
unsafeMath = vagueInts ||| unsafeMul

-- | Enable all optimizations, both safe and unsafe.
optAllUnsafe :: Config -> [String] -> Config
optAllUnsafe = optAllSafe ||| unsafeMath ||| enableWholeProgramOpts

-- | Enable all safe optimizations.
optAllSafe :: Config -> [String] -> Config
optAllSafe = updateClosureCfg

-- | Set the path to the Closure compiler.jar to use.
updateClosureCfg :: Config -> [String] -> Config
updateClosureCfg cfg ['=':arg] =
  cfg {useGoogleClosure = Just arg}
updateClosureCfg cfg _ =
  cfg {useGoogleClosure = Just closureCompiler}

-- | Add flags for Google Closure to use
updateClosureFlags :: Config -> [String] -> Config
updateClosureFlags cfg args = cfg {
  useGoogleClosureFlags = useGoogleClosureFlags cfg ++ args}

-- | Enable optimizations over the entire program.
enableWholeProgramOpts :: Config -> [String] -> Config
enableWholeProgramOpts cfg _ = cfg {wholeProgramOpts = True}

-- | Enable sloppy TCE; see Config for more info.
useSloppyTCE :: Config -> [String] -> Config
useSloppyTCE cfg _ = cfg {sloppyTCE = True}

-- | Save some space and performance by using degenerate implementations of
--   the Unicode functions.
fullUnicode :: Config -> Config
fullUnicode cfg =
    cfg {rtsLibs = unicode : filter (not . (cheap `isSuffixOf`)) libs}
  where
    libs = rtsLibs cfg
    unicode = jsDir </> "unicode.js"
    cheap = jsDir </> "cheap-unicode.js"
