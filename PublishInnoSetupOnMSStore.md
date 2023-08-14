# Publishing Inno Setup EXE installer on Microsoft Store

This is a compilation of info I collected on how to get a 
Inno Setup installer accepted.

* Note that I did not succeed yet.

MS Store now accepts EXE installers.

Requirements ( aug/23 ) :

1) The program EXE and the installer EXE must be signed with a 
Authenticode EV certificate ( those that require USB security stick when signing )

# Code signing Certificate 

Extended Validation Authenticode certificates are issued by CA authorities
like Comodo, Certum. Costs as much as $300/year.

It requires a Company with:

* Domain Name
* Contact corporate email ( same domain )
* Some form of legal company proof of existance
* Other validation checks

Not sure a person can get an EV certificate.

Once the certificate is installed, you can sign the file with 
SignTool from Windows SDK. The command looks like this:

SignTool sign /n "SomeCompany Ltd" /t "https://timestamp.comodoca.com" /fd SHA256 "path\filename.exe"

This stores the signature and timestamp on the EXE meta data.
To check the signeture, use Windows Explorer, righ-click the file and select Properties

Sign app EXE, build the installer and sign the installee file.

Place the signed installer on the company website, on a https URL ( must have a SSL certificate ).

In MS Partners website, In the application page, add a package of type EXE.

Set the installer URL.

The EXE installer  must run in complete "silence". Whence the silence test.
You can instruct Inno installers to run silently by using command line  
parameters /SILENT and /VERYSILENT.

Fill the installer package fields regarding silence parameter
and return codes. There is a Inno Setup page with the return values {URL?}.
Add the page URL to package corresponding field.

In order to pass this test, the installer execution cannot be questioned 
by Windows Defender SmartScreen either. If the execution brings a UAC
dialog saying "Windows protected your computer bla bla..", then that
installer is not suitable.

  How Smartscreen works is not clear. It performs a antivirus check
to detect known threats. If passed, the file is added to a database 
of installers with reputations. 

The installer must have certain reputation ( =a number of downloads ) 
to be allowed to run uncheked. This is a tricky requirement, as a 
typical setup changes at each new release/update. 
It can take as long as two weeks, according to one source..

It helps if you submit the file to SmartScreen ( URL? ) 

This is as far as I went. I put the project to the side for now..


