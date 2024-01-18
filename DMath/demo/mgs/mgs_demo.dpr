
{#BACKUP D:\SourceCode\Delphi\Units\MathCode\MGS_LSQ.pas}
{#BACKUP D:\SourceCode\Delphi\Units\MathCode\TestData.pas}

program MGS_Demo;

uses
  Forms,
  Dialogs,
  MGS_Test in 'MGS_Test.pas' {Form1};

{$R *.RES}

BEGIN
{$IFDEF VER80}
{$ELSE}
  Application.Initialize;
{$ENDIF}
  Application.Title := 'Modified Gram-Schmidt Demo';
  ShowMessage('© Mark Vaughan 1997, 1999.  Supplied AS IS!  Use at your own risk');
      {= I first saw this little disclaimer used in a function parser =}
      {= example sent to me by John Osborne...says everything that    =}
      {= needs saying right up front...perfect for this kind of quick =}
      {= demo app...thanks, John!                                     =}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
END.
