unit Device.Command;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes,
  Dialogs, FLSIGCTLLib_TLB, StdCtrls,
  ExtCtrls,pngimage,Data.DB, MemDS, VirtualTable;



type
 TCaptureResult=(
    crOk=0,
    crCancel=1,

    crPadError=100,
    crError=101,
    crIntegratyKeyInvalid=102,
    crNotLicenced=103,

    crAbort=200
 );

 TSignatureUtils = class
    class function Capture(Who,Why:string):TCaptureResult;
    class function SaveToPNG(aFileName:string;aAdditionalData:string=''):Boolean;
    class function SaveToBMP(aFileName:string;aAdditionalData:string=''):Boolean;
 end;

 function CaptureResultStr(r:TCaptureResult):string;

implementation
  uses Device.Licence;

var sigCtl: TSigCtl;
const Flags =RenderOutputFilename or RenderColor32BPP or RenderEncodeData;

{ TSignatureUtils }

class function TSignatureUtils.Capture(who, Why: string): TCaptureResult;
begin
 Result:= TCaptureResult(sigCtl.Capture(who, Why));
end;

function CaptureResultStr(r:TCaptureResult):string;
begin
  case r of
    crOk: Result:='Signature Captured successfully';
    crCancel: Result:='Signature Cancelled';
    crPadError: Result:='Pad Error : Signing device error';
    crError: Result:='No capture service available';
    crIntegratyKeyInvalid: Result:='Integraty Key Invalid';
    crNotLicenced: Result:='Not Licenced';
    crAbort: Result:='Abort';
  end;
end;

class function TSignatureUtils.SaveToBMP(aFileName,
  aAdditionalData: string): Boolean;
begin
  var sigObj1 := SigObj(sigCtl.Signature);
  if not aAdditionalData.IsEmpty then
      sigObj1.ExtraData['AdditionalData'] := aAdditionalData;
      sigObj1.RenderBitmap(aFileName, 200, 150, 'image/bmp', 0.5, $ff0000, $ffffff, -1.0, -1.0, Flags);
  Result:=True;
end;

class function TSignatureUtils.SaveToPNG(aFileName,
  aAdditionalData: string): Boolean;
begin
  var sigObj1 := SigObj(sigCtl.Signature);
  if not aAdditionalData.IsEmpty then
      sigObj1.ExtraData['AdditionalData'] := aAdditionalData;
  sigObj1.RenderBitmap(aFileName, 200, 150, 'image/png', 0.5, $ff0000, $ffffff, 0, 0, Flags);
  Result:=True;
end;

initialization
 sigCtl := TSigCtl.Create(nil);
 sigCtl.Licence:= WACOM_LICENCE;

finalization
 FreeAndNil(sigCtl);

end.
