{-----------------------------------------}
{          Matematikai elj†r†sok          }
{                                         }
{               Jo¢ K†roly                }
{                 RAJKA                   }
{-----------------------------------------}

UNIT JMathu ;

INTERFACE

{ Radi†nb¢l fokra †tsz†m°t†s }
FUNCTION Rad_to_Fok(Szog : Real) : Real ;

{ Fokb¢l radi†nra †tsz†m°t†s}
FUNCTION Fok_to_Rad(Szog : Real) : Real ;

{ Szîg szinusza (szîg fokban) }
FUNCTION FSin(Szog : Real) : Real ;

{ Szîg coszinusza (szîg fokban) }
FUNCTION FCos(Szog : Real) : Real ;

{ Szîg tangense (szîg radi†nban) }
FUNCTION Tang(Szog : Real) : Real ;

{ Szîg tangense (szîg fokban) }
FUNCTION FTang(Szog : Real) : Real ;

{ êrtÇk arcus szinusza (Szîg radi†nban) }
FUNCTION ArcSin(Ertek : Real) : Real ;

{ êrtÇk arcus szinusza (Szîg fokban) }
FUNCTION FArcSin(Ertek : Real) : Real ;

{ êrtÇk arcus coszinusza (Szîg radi†nban) }
FUNCTION ArcCos(Ertek : Real) : Real ;

{ êrtÇk arcus coszinusza (Szîg fokban) }
FUNCTION FArcCos(Ertek : Real) : Real ;

{ êrtÇk arcus tangense (Szîg radi†nban) }
FUNCTION ArcTang(Ertek : Real) : Real ;

{ êrtÇk arcus tangense (Szîg fokban) }
FUNCTION FArcTang(Ertek : Real) : Real ;

{ Involut fÅggvÇny kisz†m°t†sa (êrtÇk radi†nban) }
FUNCTION Inv(Ertek : Real) : Real ;

{ Egyenes szîgÇnek kisz†m°t†sa radi†nra }
FUNCTION GetLineRad(X1, Y1, X2, Y2 : Real) : Real ;

{ -------------------------- } IMPLEMENTATION { -------------------------- }

{ Radi†nb¢l fokra †tsz†m°t†s }
FUNCTION Rad_to_Fok(Szog : Real) : Real ;
  BEGIN
    Rad_to_Fok := Szog*180.0/PI ;
  END ;

{ Fokb¢l radi†nra †tsz†m°t†s}
FUNCTION Fok_to_Rad(Szog : Real) : Real ;
  BEGIN
    Fok_to_Rad := Szog*PI/180.0 ;
  END ;

{ Szîg szinusza (szîg fokban) }
FUNCTION FSin(Szog : Real) : Real ;
  BEGIN
    FSin := Sin(Szog*PI/180.0) ;
  END ;

{ Szîg coszinusza (szîg fokban) }
FUNCTION FCos(Szog : Real) : Real ;
  BEGIN
    FCos := Cos(Szog*PI/180.0) ;
  END ;

{ Szîg tangense (szîg radi†nban) }
FUNCTION Tang(Szog : Real) : Real ;
  BEGIN
    Tang := Sin(Szog)/Cos(Szog) ;
  END ;

{ Szîg tangense (szîg fokban) }
FUNCTION FTang(Szog : Real) : Real ;
  BEGIN
    FTang := Tang(Szog*PI/180.0) ;
  END ;

{ êrtÇk arcus szinusza (Szîg radi†nban) }
FUNCTION ArcSin(Ertek : Real) : Real ;
  BEGIN
    ArcSin := ArcTan(Ertek/SQRT(1-SQR(Ertek))) ;
  END ;

{ êrtÇk arcus szinusza (Szîg fokban) }
FUNCTION FArcSin(Ertek : Real) : Real ;
  BEGIN
    FArcSin := Rad_to_Fok(ArcTan(Ertek/SQRT(1-SQR(Ertek)))) ;
  END ;

{ êrtÇk arcus coszinusza (Szîg radi†nban)
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

{ êrtÇk arcus coszinusza (Szîg fokban)
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

{ êrtÇk arcus tangense (Szîg radi†nban) }
FUNCTION ArcTang(Ertek : Real) : Real ;
  BEGIN
    ArcTang := ArcTan(Ertek) ;
  END ;

{ êrtÇk arcus tangense (Szîg fokban) }
FUNCTION FArcTang(Ertek : Real) : Real ;
  BEGIN
    FArcTang := Rad_to_Fok(ArcTan(Ertek)) ;
  END ;

{ Involut fÅggvÇny kisz†m°t†sa (êrtÇk radi†nban) }
FUNCTION Inv(Ertek : Real) : Real ;
  BEGIN
    Inv := Tang(Ertek)-Ertek ;
  END ;

{ KÇt pont kîzîtti t†vols†g kisz†m°t†sa }
FUNCTION GetL(X1, Y1, X2, Y2 : Real) : Real ;
  BEGIN
    GetL := SQRT(((X2-X1)*(X2-X1))+((Y2-Y1)*(Y2-Y1))) ;
  END ;

{ Egyenes szîgÇnek kisz†m°t†sa radi†nra }
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

{ Egyenes szîgÇnek kisz†m°t†sa fokra }
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

{ H†romszîg egy szîgÇnek kisz†m°t†sa az oldalak ismeretÇben (szîg radi†nban)}
FUNCTION GetAlfa(A, B, C : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    Szog := ArcCos(((B*B)+(C*C)-(A*A))/(2.0*B*C)) ;
    IF Szog < 0 THEN Szog := Szog+PI ;
    GetAlfa := Szog ;
  END ;

{ H†romszîg egy szîgÇnek kisz†m°t†sa az oldalak ismeretÇben (szîg fokban) }
FUNCTION FGetAlfa(A, B, C : Real) : Real ;
  VAR
    Szog : Real ;
  BEGIN
    Szog := FArcCos(((B*B)+(C*C)-(A*A))/(2.0*B*C)) ;
    IF Szog < 0 THEN Szog := Szog+180.0 ;
    FGetAlfa := Szog ;
  END ;

{ H†romszîg kîrÇ irhat¢ kîr sugar†nak kisz†m°t†sa az oldalak ismeretÇben }
FUNCTION GetR(A, B, C : Real) : Real ;
  VAR
    Alfa : Real ; { radi†nban }
  BEGIN
    Alfa := GetAlfa(A, B, C) ;
    GetR := 0.5*A/Sin(Alfa) ;
  END ;

{ Real ÇrtÇk †talak°t†sa String alakra }
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