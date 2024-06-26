{-----------------------------------------}
{          Matematikai elj�r�sok          }
{                                         }
{               Jo� K�roly                }
{                 RAJKA                   }
{-----------------------------------------}

UNIT JMathu ;

INTERFACE

{ Radi�nb�l fokra �tsz�m�t�s }
FUNCTION Rad_to_Fok(Szog : Real) : Real ;

{ Fokb�l radi�nra �tsz�m�t�s}
FUNCTION Fok_to_Rad(Szog : Real) : Real ;

{ Sz�g szinusza (sz�g fokban) }
FUNCTION FSin(Szog : Real) : Real ;

{ Sz�g coszinusza (sz�g fokban) }
FUNCTION FCos(Szog : Real) : Real ;

{ Sz�g tangense (sz�g radi�nban) }
FUNCTION Tang(Szog : Real) : Real ;

{ Sz�g tangense (sz�g fokban) }
FUNCTION FTang(Szog : Real) : Real ;

{ �rt�k arcus szinusza (Sz�g radi�nban) }
FUNCTION ArcSin(Ertek : Real) : Real ;

{ �rt�k arcus szinusza (Sz�g fokban) }
FUNCTION FArcSin(Ertek : Real) : Real ;

{ �rt�k arcus coszinusza (Sz�g radi�nban) }
FUNCTION ArcCos(Ertek : Real) : Real ;

{ �rt�k arcus coszinusza (Sz�g fokban) }
FUNCTION FArcCos(Ertek : Real) : Real ;

{ �rt�k arcus tangense (Sz�g radi�nban) }
FUNCTION ArcTang(Ertek : Real) : Real ;

{ �rt�k arcus tangense (Sz�g fokban) }
FUNCTION FArcTang(Ertek : Real) : Real ;

{ Involut f�ggv�ny kisz�m�t�sa (�rt�k radi�nban) }
FUNCTION Inv(Ertek : Real) : Real ;

{ Egyenes sz�g�nek kisz�m�t�sa radi�nra }
FUNCTION GetLineRad(X1, Y1, X2, Y2 : Real) : Real ;

{ -------------------------- } IMPLEMENTATION { -------------------------- }

{ Radi�nb�l fokra �tsz�m�t�s }
FUNCTION Rad_to_Fok(Szog : Real) : Real ;
  BEGIN
    Rad_to_Fok := Szog*180.0/PI ;
  END ;

{ Fokb�l radi�nra �tsz�m�t�s}
FUNCTION Fok_to_Rad(Szog : Real) : Real ;
  BEGIN
    Fok_to_Rad := Szog*PI/180.0 ;
  END ;

{ Sz�g szinusza (sz�g fokban) }
FUNCTION FSin(Szog : Real) : Real ;
  BEGIN
    FSin := Sin(Szog*PI/180.0) ;
  END ;

{ Sz�g coszinusza (sz�g fokban) }
FUNCTION FCos(Szog : Real) : Real ;
  BEGIN
    FCos := Cos(Szog*PI/180.0) ;
  END ;

{ Sz�g tangense (sz�g radi�nban) }
FUNCTION Tang(Szog : Real) : Real ;
  BEGIN
    Tang := Sin(Szog)/Cos(Szog) ;
  END ;

{ Sz�g tangense (sz�g fokban) }
FUNCTION FTang(Szog : Real) : Real ;
  BEGIN
    FTang := Tang(Szog*PI/180.0) ;
  END ;

{ �rt�k arcus szinusza (Sz�g radi�nban) }
FUNCTION ArcSin(Ertek : Real) : Real ;
  BEGIN
    ArcSin := ArcTan(Ertek/SQRT(1-SQR(Ertek))) ;
  END ;

{ �rt�k arcus szinusza (Sz�g fokban) }
FUNCTION FArcSin(Ertek : Real) : Real ;
  BEGIN
    FArcSin := Rad_to_Fok(ArcTan(Ertek/SQRT(1-SQR(Ertek)))) ;
  END ;

{ �rt�k arcus coszinusza (Sz�g radi�nban)
FUNCTION ArcCos(Ertek : Real) : Real ;
  BEGIN
    ArcCos := ArcTan(SQRT(1-SQR(Ertek))/Ertek) ;
  END ;}

FUNCTION ArcCos(Ertek : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    IF Ertek = 0.0
      THEN Szog := PI/2
      ELSE Szog := ArcTan(SQRT(1-SQR(Ertek))/Ertek) ;
    IF Szog < 0.0 THEN Szog := Szog + PI ;
    ArcCos := Szog ;
  END ;

{ �rt�k arcus coszinusza (Sz�g fokban)
FUNCTION FArcCos(Ertek : Real) : Real ;
  BEGIN
    FArcCos := Rad_to_Fok(ArcTan(SQRT(1-SQR(Ertek))/Ertek)) ;
  END ;}

FUNCTION FArcCos(Ertek : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    IF Ertek = 0.0
      THEN Szog := 90.0
      ELSE Szog := Rad_to_Fok(ArcTan(SQRT(1-SQR(Ertek))/Ertek)) ;
    IF Szog < 0.0 THEN Szog := Szog + 180.0 ;
    FArcCos := Szog ;
  END ;

{ �rt�k arcus tangense (Sz�g radi�nban) }
FUNCTION ArcTang(Ertek : Real) : Real ;
  BEGIN
    ArcTang := ArcTan(Ertek) ;
  END ;

{ �rt�k arcus tangense (Sz�g fokban) }
FUNCTION FArcTang(Ertek : Real) : Real ;
  BEGIN
    FArcTang := Rad_to_Fok(ArcTan(Ertek)) ;
  END ;

{ Involut f�ggv�ny kisz�m�t�sa (�rt�k radi�nban) }
FUNCTION Inv(Ertek : Real) : Real ;
  BEGIN
    Inv := Tang(Ertek)-Ertek ;
  END ;

{ K�t pont k�z�tti t�vols�g kisz�m�t�sa }
FUNCTION GetL(X1, Y1, X2, Y2 : Real) : Real ;
  BEGIN
    GetL := SQRT(((X2-X1)*(X2-X1))+((Y2-Y1)*(Y2-Y1))) ;
  END ;

{ Egyenes sz�g�nek kisz�m�t�sa radi�nra }
FUNCTION GetLineRad(X1, Y1, X2, Y2 : Real) : Real ;
  VAR
    Data : Real ;
  BEGIN
    IF (X1 = X2) THEN IF (Y2 > Y1) THEN Data := PI/2 ELSE Data := PI*3/2 ;
    IF (Y1 = Y2) THEN IF (X2 > X1) THEN Data := 0.00 ELSE Data := PI ;
    IF (X1=X2) AND (Y1=Y2) THEN Data := 0.00 ;
    IF (X2 <> X1) AND (Y2 <> Y1) THEN
      BEGIN
        Data := ArcTan((Y2-Y1)/(X2-X1)) ;
        IF (X2 < X1) THEN Data := Data + PI ;
        IF Data < 0 THEN Data := Data + (2*PI) ;
      END ;
    GetLineRad := Data ;
  END ;

{ Egyenes sz�g�nek kisz�m�t�sa fokra }
FUNCTION GetLineFok(X1, Y1, X2, Y2 : Real) : Real ;
  VAR
    Data : Real ;
  BEGIN
    IF (X1 = X2) THEN IF (Y2 > Y1) THEN Data := PI/2 ELSE Data := PI*3/2 ;
    IF (Y1 = Y2) THEN IF (X2 > X1) THEN Data := 0.00 ELSE Data := PI ;
    IF (X1=X2) AND (Y1=Y2) THEN Data := 0.00 ;
    IF (X2 <> X1) AND (Y2 <> Y1) THEN
      BEGIN
        Data := ArcTan((Y2-Y1)/(X2-X1)) ;
        IF (X2 < X1) THEN Data := Data + PI ;
        IF Data < 0 THEN Data := Data + (2*PI) ;
      END ;
    GetLineFok := Rad_to_Fok(Data) ;
  END ;

{ H�romsz�g egy sz�g�nek kisz�m�t�sa az oldalak ismeret�ben (sz�g radi�nban)}
FUNCTION GetAlfa(A, B, C : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    Szog := ArcCos(((B*B)+(C*C)-(A*A))/(2.0*B*C)) ;
    IF Szog < 0 THEN Szog := Szog+PI ;
    GetAlfa := Szog ;
  END ;

{ H�romsz�g egy sz�g�nek kisz�m�t�sa az oldalak ismeret�ben (sz�g fokban) }
FUNCTION FGetAlfa(A, B, C : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    Szog := FArcCos(((B*B)+(C*C)-(A*A))/(2.0*B*C)) ;
    IF Szog < 0 THEN Szog := Szog+180.0 ;
    FGetAlfa := Szog ;
  END ;

{ H�romsz�g k�r� irhat� k�r sugar�nak kisz�m�t�sa az oldalak ismeret�ben }
FUNCTION GetR(A, B, C : Real) : Real ;
  VAR
    Alfa : Real ; { radi�nban }
  BEGIN
    Alfa := GetAlfa(A, B, C) ;
    GetR := 0.5*A/Sin(Alfa) ;
  END ;

{ Real �rt�k �talak�t�sa String alakra }
FUNCTION RealToStr( Ertek : Real ;
                    Hossz, Tizedes : Byte) : String ;
  VAR
    KiStr : String ;
  BEGIN
    Str(Ertek:Hossz:Tizedes, KiStr) ;
    RealToStr := KiStr ;
  END ;

BEGIN
END.