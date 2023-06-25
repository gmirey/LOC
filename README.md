![LocLogo](https://github.com/gmirey/LOC/assets/57968638/3ad47e5b-3d17-4b21-9895-d1025d71ebc1)
# LOC
The LOC language

LOC is an imperative, structured, procedural programming language, as is, for example, C.

It has a tokenless block syntax (meaningful indentation, restricted to *tabs*), it makes more assumption than C does about your machine, and enforces more default, safer behaviours. Getting out of these safe behaviours (in search of raw performance, comparable to C) is possible, where you want it - and where it matters most - through the use of precise operators, explicit builtins, or advanced annotations.

Its aim is to be very fast-to-compile, and visually attractive enough that it becomes a sensible choice for newcomers to *learn* about programming, all the while being still a serious, and hopefully productive, performant, *compiled* language, staying close to the machine...

On your way from novice to master, it shall also allow you to exert more and more *control* on the resulting binary - the LOC compiler becoming a clear and precise *tool*, to produce machine code. 

---

This initial commit is a very very early, coarse, unpolished version, of something beginning to look like a LOC compiler, with which you can create your first programs. MANY features are still missing... even floating point support is lacking!

---

Disclaimer: I have yet to test this code anywhere else than on my machine, which has an x64 proc and is running Windows 10, with Visual Studio 2022 installed. If you have a similar configuration, then you can do as I do:

I'm simply opening a command prompt (cmd.exe), configured to run the provided "shell2022.bat" at start, as Casey demonstrated in day 001 of his 'Handmade Hero' series - you may want to edit that to point to your path for MSVC (note: I indeed switched to VS2022 at some point... can't remember if the "feature" I needed 2022 for - instead of the previous 2019 or even 2017 - is really something used in that release. But I'll advise 2022 still as this is precisely what I'm developping with currently)

A few things have been ("prematurely-"?)designed in that codebase to allow for an easier port to nix* platforms, though... but such a port is NOT YET SUPPORTED. 

---

How to (currently, and painfully) use:

Once you're setup with the correct path to MSVC, and you're in the directory where you copied this repo, simply run "build.bat" within that cmd.exe console to build the loc compiler from source. It shall produce a 'loc.exe' (which is the windows version of the command-line executable to launch the loc compiler) and 'loc.dll' (which has the code for the compiler, proper), both in the 'build' subfolder.
Then, you can run "compilesample.bat", which is configured to launch the compiler against whatever file is called "sample_main.loc" in the 'sample' subfolder. Nothing is yet setup to configure a custom output name, so on compilation success, this shall output a file named 'testWinX64Backend.exe'. This is the resulting executable.

you can run "rundirectsample.bat" from there, to launch that 'testWinX64Backend.exe' binary within the 'run' subfolder, as default *current folder* for that execution.

---

This initial commit is configured in debug mode, with most of my current debugging bells-and-whistles still active, and runs single-thread. So it won't surely feel that fast, and produce various dev-only outputs: an ir-dump, and an almost-full-traces log, along the way. It also has asserts all over the place. Despite this, stability is not guaranteed (not even proper terminations through those 'asserts'... there are still ways in which it may still plain crash).

The resulting executable's stability is not guaranteed either. The auto-checks specific to LOC language rule enforcement (arithmetic overflows, table indices...) which are currently implemented would trigger a halt, but there is no polished handler atm which would warn the end user about it. And it may just plain crash, also (why not).

'LOC-syntax-notepad++.xml' is provided for a crude syntax-coloring scheme for LOC, with notepad++. It is *FAR* from perfect as it is.

---

Where to find a (little) more info, before any documentation is available:

LOC as a 'Project' page on handmade.network : https://handmade.network/p/403/loc-language/

LOC introduction thread on the forums : https://handmade.network/forums/wip/t/8715-introducing_the_loc_language
