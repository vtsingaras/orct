# Open Radio Calibration Toolkit

An enhanced Open Source Implementation to replace Qualcomm's QRCT

## Object

Mobile  voice  and data  modem  chipsets  come  with extensive  calibration  and
configuration  data sets  usually  provided by  proprietary  software tools  and
encoding formats. The Open Radio Calibration Toolkit (ORCT) aims to address some
short-comings of the  official Qualcomm Radio Calibration Toolkit  which is used
for most Qualcomm modems and futhermore seems not to be maintained anymore. This
open source  project can be therefore  rather useful for Qualcomm  licensees who
like to stay  with the very well  established, stable and wide  spread XML based
generation process of mobile configuration data.

In  this context  mobile  configuration  data also  known  as non-volatile  (NV)
parameter set is encoded in a proprietary XML format which is transformed into a
binary representation,  also denoted  as QCN  file format.  QCN stands  for QPST
content,  an acronym  for  the  Qualcomm Product  Support  Tool software  suite.
QPST  includes a  software  download  utility which  allows  to quickly  install
configuration data incrementally or to backup the overall calibration data which
is already installed on  a modem chip.

While this  approach looks very  attractive on paper, the  actual implementation
does not comply to recent needs. In  example Voice over LTE (VoLTE) requires the
definition of several URL addresses as  ASCII character strings, but QRCT is not
capable of  processing ASCII  data at  all. QRCT is  furthermore not  capable of
managing  the representation  of the  more complex  file system  based parameter
structures and instead presents these parameters  as a big binary blob. It lacks
also proper  error handling and  type checking. When  a paramter is  not exactly
specified as expected, it is simply not written to the QCN output data. Last but
not least QRCT is closed source and the program is exclusively available for the
Windows platform.  This is especially  anoying within Android/Linux  based build
infrastructures since  the QCN  paramter sets  need to  be generated  and tested
outside the used build tool chain.

ORCT aims to fully replace QRCT's XML transformations. It is

* considered to be fully downard compatible to QRCT
* sits on the JVM and runs on all major platforms
* provides a consise functional implemenation of its core logic
* is deployed under the terms of the GNU General Public License

More recently the QCN file format is more and more replaced by the Mobile Binary
Configuration (MBN)  file format which is  based on an ELF  container format and
thus  more  easier  to process  on  a  target  platform.  While MBN  comes  with
friendlier  compilation tools  where parameters  can  be entered  as clear  text
within an Excel sheet,  there currently no tools to view the  contents of an MBN
file. For this  reason we included a MBN  parser which can be used  to print out
and diff an configuration for which only a MBN file is available.

This program is distributed in the hope  that it will be useful, but WITHOUT ANY
WARRANTY.


## Usage

The software is written in the [Clojure programming language](http://clojure.org)
which requires a Java virtual machine and some additional libraries for operation.

### Build

You need  the clojure build  tool
[leinignen](https://github.com/technomancy/leiningen) for  compilation. Download
the lein script file from Github

    $ cd ~/bin
    $ wget http://github.com/technomancy/leiningen/raw/stable/bin/lein
    $ chmod +x lein

and type

    $ lein self-install

The following commands will generate and stand-alone jar file when executed from
orct's project root directory:

    $ lein uberjar

Refer also to [Zef's Leiningen page](http://zef.me/2470/building-clojure-projects-with-leiningen)
for more specific information about build options.


### Invocation
The following command will start the generation of an qcn output file

    $ java -jar orct-<xyz>-standalone.jar -s <schemafile.xml> -c <masterfile.xml> <outputfile.qcn>

The required arguments are:

* schemafile.xml: schema definition in QRCT's proprietary XML format, refer to samples/NvDefintion.xml
* masterfile.xml: main nv item defintion file, can include futher XML files

The application can be also started by the script script/orct. It is recommended
to install this  script together with orct's  standalone jar file to  one of the
directories of the standard execution path e.g. /usr/local/bin or /usr/bin. This
allows to invoke  orct with a much shorted command  line. The following snippets
illustrate  typical use  cases. When  setting the  current working  directory to
orct/samples, they shall be all operational out of the box:

    $ orct -s NvDefinition.xml -p sample.qcn                # writes the content of sample.qcn to standard out
    $ orct -s NvDefinition.xml -p Masterfile.xml            # writes the content of Masterfile.xml to standard out
    $ orct -s NvDefinition.xml -c Masterfile.xml test.qcn   # compiles Masterfile.xml into test.qcn
    $ orct -s NvDefinition.xml -d Masterfile.xml test.qcn   # show the difference of Masterfile's and test's nv item setup
    $ orct --help                                           # prints help screen

The above diff-operation will deliver an empty set in this case. It requires the
precense of the tool 'diff' which is found on most Unix platforms. The option -t
allows to specify an alternative diff-tool e.g. meld, beyond \& compare, etc.

## Licence
This software stands under the terms of the
[GNU General Public Licence](http://www.gnu.org/licenses/gpl.html).

## Resources and links
Thanks to  all the  giants whose shoulders  we stand on.  And the  giants theses
giants stand on... And special thanks to Rich Hickey (and the team) for Clojure.
Really, thanks!

* Clojure: http://clojure.org
* Leiningen: https://github.com/technomancy/leiningen

Copyright © 2015 Otto Linnemann