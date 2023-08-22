# Publishing Inno Setup EXE installer on Microsoft Store

Notes collected on how to get a Inno Setup EXE installer accepted on the MS Store.

( I did not succeed yet )

MS Store now accepts EXE installers.

Current Requirements ( aug/23 ) :

1) The program EXE and the installer EXE must be signed with a 
Authenticode EV certificate ( those that require USB security stick when signing )

2) The installer must run silently

# Code signing Certificate 

Extended Validation Authenticode certificates are issued by CA authorities
like Comodo, Certum. Costs as much as $300/year.

It requires a Company with:

* Company domain Name
* Company website with SSL certificate
* Contact email ( corporate email on the same domain )
* Legal company proof of existence
* Other validation checks

Not sure a person can get an EV certificate.

# Signing files

Once the certificate is issued and installed on the computer, 
you can sign files with SignTool from Windows SDK. 

Install Windows SDK if needed. Typical Windows SDK path is 

    \Program Files (x86)\Windows Kits\10\bin\10.0.1234.0\x86"              

The sign command looks like this:

    SignTool sign /n "SomeCompany Ltd" /t "http://timestamp.comodoca.com" /fd SHA256 "path\filename.exe"

This stores the signature and timestamp on the EXE file meta data.

* To check the signature, use Windows Explorer, righ-click the file and select Properties.

Sign the app executable, build the installer and sign the installer EXE.
Name the installer something like "setupMyApp-Ver11-Win32.exe"

Place the signed installer on the company website, on a https URL ( must have a SSL certificate ).

On Microsoft Partners website, on the application page, add a package of type EXE.

Set the installer URL.

# Silence test

The EXE installer  must run in complete "silence". Whence the silence test.
You can instruct Inno installers to run silently by using command line  
parameters /SILENT and /VERYSILENT.

Fill the installer package fields regarding silence parameter
and return codes. There is a Inno Setup page with the return values.
Add the page URL to package corresponding field.

# Windows Defender 

In order to pass the test, the installer execution cannot be questioned 
by Windows Defender SmartScreen either. If the execution brings a UAC
dialog saying "Windows protected your computer bla bla..", then that
installer is not suitable.

The problem is all installers must run with some administrative rights
to be able to create directories, add files to \Program Files\,
add data files to user Documents, create shortcuts. 
Not things Windows Defender likes...

The installer cannot ask for user options either. Install with default options.

How Smartscreen works is not clear. It performs a antivirus check
to detect known threats. If passed, the file is added to a database 
of installers with reputations. 

The installer must have certain reputation ( =a number of downloads ) 
to be allowed to run unchecked. This is a tricky requirement, as a 
typical setup changes at each new release/update. 
It can take as long as two weeks, according to one source..

It helps if you submit the file to SmartScreen ( URL? ) 

This is as far as I went. I put the project to the side for now..



