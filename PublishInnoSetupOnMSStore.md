# Publishing Inno Setup EXE installer on Microsoft Store

Notes collected on how to get a Inno Setup EXE installer accepted on the MS Store.

! This a little off-topic in this astronomy/graphics repo !

Software installers are programs that you run to setup a software package.  
Usually run once to install the software components, default Documents files,
create app shortcuts. Inno Setup is a very popular installer builder
for Windows platform ( Win32 ).

MS Store now accepts EXE installers.

Current Requirements ( aug/23 ) :

1) The app EXEcutables and the installer EXE must be signed with a code-signing certificate

       note: according to this post by Jernej Simončič, non-EV certificates are also accepted.
       ( I previously said that EV certificate was required )

2) The installer must run silently (i.e. show no user options and no UAC dialog )   

# Code signing Certificate 

EV and Non-EV certificates are accepted on the MS Store.

Extended Validation Authenticode certificates are issued by CA authorities
like Comodo, Certum. Costs as much as $300/year.

EV certificate requires a Company with:

* Company domain Name
* Company website with SSL certificate
* Contact email ( corporate email on the same domain )
* Legal company proof of existence
* Other validation checks
* Signing uses 2 factor authentication ( password and a external hardware token )

Non-EV certificate signing requires only the password. Use a large and hard to break password.
Keep it on external drive, not anywhere in the computer. 

# Signing files

According to MS store policies:

    "10.2.9.b - The binary and all of its Portable Executable (PE) files must be digitally 
    signed with a code signing certificate that chains 
    up to a certificate issued by a Certificate Authority (CA) that 
    is part of the Microsoft Trusted Root Program."

Portable Executable (PE) means all executable code ( extensions EXE, DLL, CPL, SRV ).

Once the code signing certificate is issued and installed on the computer, 
you can sign files with SignTool from Windows SDK. 

Install Windows SDK if needed. Typical Windows SDK path is 

    \Program Files (x86)\Windows Kits\10\bin\10.0.1234.0\x86"              

The sign command with EV certificate looks like this:

    SignTool sign /n "YourCompany Ltd" /t "http://timestamp.comodoca.com" /fd SHA256 "path\filename.exe"

Make sure the company name matches the certificate. At this point you will be prompted for the password and token ( 2 factor authentication )

If signing with a non-EV certificate, the password can be included in the command line:

    SignTool sign /f "path\YourCompanyCertificate.pfx" /p "aLargePassword" /t "http://timestamp.comodoca.com" /fd SHA256 "path\filename.exe"

SignTool stores the digital signature and timestamp on the executable file meta data.

* To check the signature, use Windows Explorer, righ-click the file and select Properties.

Sign the app executables, build the installer and sign the installer EXE.
Name the installer something like "setupMyApp-Ver11-Win32.exe" ( that is, with app name and version number )
Note that, once accepted, the installer cannot be modified. If you release a new version,
change the installer filename.

Place the signed installer on the company website, on a https URL (i.e. the webserver must have a SSL certificate ).
Once the app is submitted, the file cannot be modified.

On Microsoft Partners website, create a developer account.
Add a new application. Provide app name, description,
store images, license, privacy rules. Content rating. User data usage. App price. etc. 
Sign all required contracts. 80% of the work here.

Add a package of type EXE.

Set the installer URL.

# Silence test

The EXE installer  must run in complete "silence". Whence the silence test.
You can instruct Inno installers to run silently by using command line parameter /VERYSILENT
( a simple /SILENT didn't work because it displayed a progress bar )

Fill the installer package fields regarding silence parameter
and return codes. There is a Inno Setup page with the return values.
Add the page URL to package corresponding field.

# Windows Defender SmartScreen

In order to pass the test, the installer execution cannot be questioned 
by Windows Defender SmartScreen either. If the execution brings a UAC
dialog saying "Windows protected your computer bla bla..", then that
installer is not suitable.

The problem is all installers must run with some administrative rights
to be able to create directories, add files to \Program Files\,
add data files to user Documents, create shortcuts. 
Not things Windows Defender likes...

The installer cannot ask for user options either. Install with default options.

Smartscreen performs some anti-virus checks to detect known threats. 
If passed, the file is added to a database of installers with reputations,
starting with 0.

The installer must have certain reputation (i.e. a certain number of downloads ) 
to be allowed to run unchecked. This is a tricky requirement, as a 
installer changes at each new release/update. 

Both the installer and the certificate used to sign it have reputations
that improve with downloads and also as the time passes.
Certificate reputations take longer.  

Before testing your installer on MS Partners, it must be submitted to
Windows Defender SmartScreen ( the Windows anti-virus ). 
In my case, it took about 1 week to the app to be accepted by SmartScreen.
After that, the app was also accepted on the MS Store.
So we have this 2 tier system on this platform.

The store check is also in 2 parts. You can perform a basic test on 
your installer package before submiting to publication.
The store review takes a couple days.

* Apps on MS Store:

* Nassau St - https://apps.microsoft.com/store/detail/nassau-st/XPFCXKJ048XRHF

* Planet Fun - https://apps.microsoft.com/store/detail/planet-fun/XPFD2CLDB2Q8S4


## Links

* MS Partners website:  https://partner.microsoft.com    <--- Manage ms store apps here
* MS Store installer requirements: https://learn.microsoft.com/en-us/windows/apps/publish/store-policies
* Inno setup: https://jrsoftware.org  <-- Great installer builder
* Inno setup exit codes: https://jrsoftware.org/ishelp/topic_setupexitcodes.htm
* Inno setup command line parameters: https://jrsoftware.org/ishelp/index.php?topic=setupcmdline
* Inno setup Forum: https://groups.google.com/g/innosetup/c/f2-s8egy4s0
* Submit file to SmartScreen https://www.microsoft.com/en-us/wdsi/filesubmission  (select Windows Defender Smartscreen) 
  

Omar - Aug/23




