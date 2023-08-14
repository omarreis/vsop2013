# Publishing Inno Setup EXE installer on Microsoft Store

MS Store now accepts EXE installers.
This is a compilation of info I collected on how to get an installer accepted.

* Note that I did not succeed yet.

* The program EXE and the installer EXE must be signed with a 
Authenticode EV certificate ( those that require USB security stick when signing )

# Authenticode EV (Extended Validation) certificates are issued by CA authorities,
like Comodo, Certum. Costs as much as $300/year.

It requires a Company with:
* Domain Name
* Contact corporate email ( same domain )
* Some form of legal company proof of existance

Not sure it can be a person can get a EV certificate.

The EXE must run in complete "silence". Whence the silence test.
You can instruct Inno installers to run silently by using command line  
parameters /SILENT and /VERYSILENT.

Once the certificate is installed, you can sign the file with 
SignTool ( from Windows SDK). The command looks like this:

SignTool sign /n "SomeCompany Ltd" /t "http://timestamp.comodoca.com" /fd SHA256 "path\filename.exe"

Sign app EXE, build the installer and sign the EXE file.

Place the signed installer on the company website, on a https URL.

In MS Partners website, In the application page, add a package type to EXE.

Set the installer URL.

Fill the installer package fields regarding silence parameter
and return codes. There is a Inno Setup page with the return values.
Add the page URL to package field.

  In order to pass this test, the installer execution cannot be questioned 
by Windows Defender SmartScreen. If the execution brings a UAC
dialog saying "Windows protected your computer bla bla..", then that
installer is not suitable.

  How Smartscreen works is not clear. It performs a antivirus check
to detect known threats. If passed, the file is added to a database 
of installers with reputations. 

The installer must have certain reputation ( =a number of downloads ) 
to be allowed to run. This is a tricky requirement, as a 
typical setup changes at each new release/update. 

Can take as long as two weeks, according to one source..

It helps if you submit the file to SmartScreen.

This is as far as I went. I put the project to the side for now..


