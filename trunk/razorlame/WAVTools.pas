unit WAVTools;


{

 OK, so the idea here is that we load as _little_ of the file
 as possible.

 The header is loaded in during the constructor, but the actual
 sound data is left alone....


 filename is private: idea is it cannot be changed.


 }

interface



uses
    windows, UtilFuncs, mmsystem, SysUtils;

type

    TWavFileHeader = record
        lRiff,        
        lFileSize,    
        lWave,        
        lFormat,      
        lFormatLength: LongInt;
    end;

    //use of shorts here goes against the Win32 API documentation.
    //I think Delphi Integer is different number of bits to VC++ Integer....:)
    TMyWavFormat = record  //notice: different _order_ to TWaveFormat!!! this is MS stupidity
        wFormatTag         : Short;            //need short       = 8 bit;
        nChannels          : Short;
        nSamplesPerSec     : Integer;          //need Integer     = 32 bit;
        nAvgBytesPerSec    : Integer;
        nBlockAlign        : Short;
        nBitsPerSample     : Short;
    end;

    TChunkHeader = record
        lType              : Integer;
        lLen               : Integer;
    end;


    TWavFile = class(TObject)
      private

        WaveFormat:TMyWavFormat;
        FileHeader:TWavFileHeader;

        function FGetDuration     : Real;
        function FGetFileLength   :LongInt;
        function FGetSamplesPerSec:LongInt;
        function FGetBitsPerSample:LongInt;

      public
        valid:boolean;

        constructor Create(const asFname:string); { create object }

        //helper functions
        property FileLength   : LongInt read FGetFileLength;
        property Duration     : Real    read FGetDuration;
        property SamplesPerSec: LongInt read FGetSamplesPerSec;
        property BitsPerSample: LongInt read FGetBitsPerSample;


        //high level functions
        procedure SaveExcerpt(const asFile:string; const StartTime, EndTime:Real);


      private

        FileName:string;

    end;

implementation


     constructor TWavFile.Create(const asFname:string);
     
     var
          Datafile: file;
          liNumRead: Integer;

     begin
          inherited Create;

          valid    := true;
          FileName := asFname;

          FillChar(WaveFormat, sizeof(WaveFormat), 0);
          FillChar(FileHeader, sizeof(FileHeader), 0);

          AssignFile(Datafile, asFname);
          try
             FileMode := 0;
             Reset(Datafile, 1);

             BlockRead(Datafile, FileHeader, sizeof(FileHeader), liNumRead);
             if liNumRead <> sizeof(FileHeader) then valid := false;

             BlockRead(Datafile, WaveFormat, sizeof(WaveFormat), liNumRead);
             if liNumRead <> sizeof(WaveFormat) then valid := false;

             if FileHeader.lRiff <> $46464952 then valid := false;   //lRiff = 'RIFF'
             if FileHeader.lWave <> $45564157 then valid := false;   //lWave = 'WAVE'


          finally
             CloseFile(Datafile);
          end;

     end;

     function TWavFile.FGetFileLength : LongInt;
     begin
          result := GetFileSize(FileName);
     end;

     function TWavFile.FGetDuration : real;
     begin
         result := FileLength / WaveFormat.nAvgBytesPerSec;
     end;

     function TWavFile.FGetSamplesPerSec:LongInt;
     begin
          result := WaveFormat.nSamplesPerSec;
     end;

     function TWavFile.FGetBitsPerSample:LongInt;
     begin
          result:= WaveFormat.nBitsPerSample;
     end;


     
     //times are in seconds after 0 seconds.
     procedure TWavFile.SaveExcerpt(const asFile:string; const StartTime, EndTime:Real);
     var
         
          DataFile, ExFile: file;

          pBuff:PChar;
          ExHeader:TWavFileHeader;
          ExChunk:TChunkHeader;

          liNumRead: Integer;
          liDataSize,liDataOffset:Integer;

          nStart, nEnd:Real;

          BytesPerSample:Integer;

     begin
             if not valid then exit;

             nStart := StartTime;
             nEnd   := EndTime;

             if nStart < 0    then nStart := 0;
             if nEnd < nStart then nEnd := nStart;


             //test
             AssignFile(Datafile, FileName);
             FileMode := 0;
             Reset(Datafile, 1);

             //want an integral number of samples...
             BytesPerSample := WaveFormat.nBitsPerSample div 8;
             liDataSize := BytesPerSample*(Trunc(WaveFormat.nAvgBytesPerSec * (nEnd - nStart)) div BytesPerSample);
             liDataOffset:= sizeof(FileHeader) + FileHeader.lFormatLength +
                            BytesPerSample*(Trunc(WaveFormat.nAvgBytesPerSec * nStart) div BytesPerSample);

             ExHeader := FileHeader;
             ExHeader.lFileSize := sizeof(TWavFileHeader) + sizeof(TMyWavFormat)
                                  + liDataSize;

             AssignFile(ExFile, asFile);
             Rewrite(ExFile, 1);

             BlockWrite(ExFile,ExHeader,sizeof(ExHeader));
             BlockWrite(ExFile,WaveFormat,sizeof(WaveFormat));


             pBuff := AllocMem(liDataSize);
             try
                Seek(DataFile,sizeof(FileHeader) + FileHeader.lFormatLength);
                BlockRead(DataFile, ExChunk, sizeof(ExChunk));
                ExChunk.lLen := liDataSize;

                Seek(DataFile,sizeof(FileHeader) + FileHeader.lFormatLength+ liDataOffset);
                BlockRead(DataFile,pBuff^, liDataSize, liNumRead);
                BlockWrite(ExFile, ExChunk, sizeof(ExChunk));
                BlockWrite(ExFile,pBuff^,liNumRead);         //possibly not equal to liDataSize
             finally
               FreeMem(pBuff);
               CloseFile(Datafile);
               CloseFile(ExFile);
             end;
     end;

end.

