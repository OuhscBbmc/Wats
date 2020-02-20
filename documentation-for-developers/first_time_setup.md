These notes are to hep potential REDCapR contributors get their development machines set up correctly to build the package.  It's also to help me remember how to do it if I switch to another computer.  Nothing mysterious -just little things to save 10 minutes here and there.  Unfortunately it's not as easy as installing R and RStudio and clicking `Build`.

### R
Install the [latest version](https://cran.r-project.org/bin/windows/base/) of R.  Before a version is submitted to CRAN, the CRAN maintainers prefer you use the [patched](https://cran.rstudio.com/bin/windows/base/rpatched.html) version or R, or even the [development](https://cran.rstudio.com/bin/windows/base/rdevel.html) version.  These links point to the windows versions.  If you're running Linux, you probably already know how to get the right version, and there's not a simply URL I can link to.

### RStudio
Although other R IDEs are available, using [RStudio](https://www.rstudio.com/ide/download/desktop) will be easier when developing REDCapR, since that the major link of the tool chain I'm used to.

### Rtools
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is the main thing that builds and checks the package being developed.  CRAN maintainers like you to run the most recent version.

### LaTeX  software
For Windows, the consensus seemed to be [MiKTeX](http://miktex.org/download).  The full version is huge, and contains almost every possible package (LaTeX has a package system similar to R and Linux).  If you install something other than the full version, and a document requires a package you don't have, it will simply prompt you to download it.

### qpdf
[qpdf](http://sourceforge.net/projects/qpdf/) is a command line tool that R BUILD calls.  You'll never call it directly.  It is used to make see if the documentation pdfs can be further compressed to save additional space.  For Windows, download the zipped file from sourceforge and copy it into a permanent location (eg, in the `/Program Files/` directory.)  

The location of the `bin` directory should be added to the OS's "PATH" environmental variable, so the executable can be found. At the time of this writing, I added `C:\Program Files\qpdf-5.0.1\bin;`.  Make sure a semicolon is separating it from all the stuff before it in the PATH variable.  Make sure the version number (eg, "5.0.1") matches the version of your current installation. *Apparently I needed to reboot my computer for the new path entry to be recognized (see [a stack exchanage explanation for tedious details](http://serverfault.com/a/557669)); restarting RStudio didn't work.*

If you don't install qpdf correctly, you'll likely receive the error message `WARNING 'qpdf' is needed for checks on size reduction of PDFs` when running R CHECK with the `--as-cran-` option.

### R Packages
We try to keep a current list of all the packages used in REDCapR in the file `/utility_scripts/package_dependencies.R`.  This is partly for the sake of documentation, and partly so there aren't any surprises for new contributors.  Just run this file, and everything should be installed if it's not already.  After this script is run the first time, make sure you click the 'Check for Updates' button in the 'Packages' every week or so.

### Help me add more to this
This document is mostly from memory, except for the qpdf which was a waste of 10 minutes of re-googling.  There's probably other stuff I've forgotten, that would cost time.  If so, please tell me and I'll add it here.

### SVN
*Ignore this section.  It didn't work out like I had hoped.  I couldn't get SVN to ignore directories. Maybe it involved conflicts between Git, and holding locks or something.*
If you're going to be submitting to R-Forge, you can use TortoiseSVN or the [SVN feature in RStudio](http://www.rstudio.com/ide/docs/version_control/overview). The RStudio Suggests [SilkSVN](http://www.sliksvn.com/en/download).  SVN ignores files differently than Git.  Open a command window in the appropriate directory, and [type](http://stackoverflow.com/questions/116074/how-to-ignore-a-directory-with-svn
```
svn propset svn:ignore .git .
svn propset svn:ignore .Rproj.user .
```
Likewise, add the following line to ./.gitignore:
```
.svn
```
