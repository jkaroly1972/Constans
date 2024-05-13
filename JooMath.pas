    //=======================================================================//
   //                                                                       //
  //     M A T E K   -  M a t h e m a t i s c h e   F u n k t i o n e n    //
 //                                                                       //
//=======================================================================//

unit JooMath;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs ;

{-----------------------------------------}
{  Konstansok és adattípusok definíciója  }
{-----------------------------------------}

TYPE
  { 2D koordináta pontok típusa }
  K2D = RECORD
          X : Real ;
          Y : Real ;
        END ;

  { 3D koordináta pontok típusa }
  K3D = RECORD
          X : Real ;
          Y : Real ;
          Z : Real ;
        END ;

  { 2D polárkoordináta típusa }
  P2D = RECORD
          Gamma : Real ;  { X tengellyel bezárt szög }
          L : Real ;      { távolság az Origótól     }
        END ;

  { 3D polárkoordináta típusa }
  P3D = RECORD
          Gamma : Real ;  { X tengellyel bezárt szög }
          Theta : Real ;  { XY síkkal bezárt szög    }
          L : Real ;      { távolság az Origótól     }
        END ;


{-------------------------------------------}
{  Alap szögfüggvények, "fok" paraméterrel  }
{-------------------------------------------}

FUNCTION FSIN(Alfa : Real) : Real ;

FUNCTION FCOS(Alfa : Real) : Real ;

FUNCTION FArcCos(E : Real) : Real ;

FUNCTION FTAN(Alfa : Real) : Real ;


{ 2D-ben pont forgat s Orig˘ k”rl Alfa sz”ggel (fok) }
PROCEDURE FORG2DO( Pbe : K2D ;
                  VAR Pki : K2D ;
                  Alfa : Real ) ;

{ 2D-ben pont forgat s megadott "Pc" pont k”rl }
PROCEDURE FORG2D( Pc : K2D ;
                  Pbe : K2D ;
                  VAR Pki : K2D ;
                  Alfa : Real );

{ 3D-ben pont forgat sa Orig˘ k”rl }
PROCEDURE FORG3DO( Pbe : K3D ;
                   VAR Pki : K3D ;
                   Gx, Gy, Gz : Real ) ;


{ 3D-ben pont forgat sa Pc b zispont k”rl }
PROCEDURE FORG3D(  Pc : K3D ;
                   Pbe : K3D ;
                   VAR Pki : K3D ;
                   Gx, Gy, Gz : Real ) ;

{ ArcTan fggv‚ny amely "fok" ‚rt‚ked ad vissza }
FUNCTION FArcTan(E : Real) : Real ;

{ 2D pont t vols g t adja vissza az orig˘t˘l }
FUNCTION Length2DO(P : K2D) : Real ;

{ két 2D pont távolságát adja vissza }
FUNCTION Length2D(P1, P2 : K2D) : Real ;

{ 3D pont távolságát adja vissza az origótól }
FUNCTION Length3DO(P : K3D) : Real ;

{ két 3D pont távolságát adja vissza }
FUNCTION Length3D(P1, P2 : K3D) : Real ;

{ két egyenes "A"‚s "B"  ltal bez rt sz”g kisz mˇt sa = inv. Cosinus t‚tel }
{ Gamma a "C" oldallal szembeni sz”g }
FUNCTION GetGamma(A, B, C : Real) : Real ;

{ 3D-ben két egymást követő 3 egyenes szakasz által bezárt szög számítása
  = harmadik oldallal szembeni szög = Biegewinkel }
FUNCTION GetBiegewinkel(P1, P2, P3 : K3D) : Real ;

{ 2D koordináták átalakítása polárkoordinátákká, Origóhoz képest}
PROCEDURE GetPolar2DO( P : K2D ;
                       VAR Pol: P2D) ;

{ 2D koordin t k  talakˇt sa pol rkoordin t kk , Pc b zisponthoz k‚pest}
PROCEDURE GetPolar2D( Pc : K2D ;
                      P : K2D ;
                      VAR Pol: P2D) ;

{ 3D koordin t k  talakˇt sa pol rkoordin t kk  orig˘hoz k‚pest }
PROCEDURE GetPolar3DO( P : K3D ;
                       VAR Pol : P3D ) ;

{ 3D koordin t k  talakˇt sa pol rkoodin t kk  Pc b zisponthoz k‚pest   }
PROCEDURE GetPolar3D( Pc : K3D ;          { b zispont                   }
                      P : K3D ;           { Geopont                     }
                      VAR Pol : P3D ) ;   { sz mˇtott pol r koordin t k }

{ Orig˘ban l‚v‹ pol r koordin ta  talakˇt sa 3D-koordin t kk  }
PROCEDURE GetKoord3DO( PP : P3D ;
                       VAR PK : K3D ) ;

{ PK ponthoz k‚pes PP Pol r koordin ta  talakˇt sa 3D-koordin t kk  }
PROCEDURE GetKoord3D( PK : K3D ;
                      PP : P3D ;
                      VAR PK2 : K3D ) ;

{ Sˇknegyedekkel bez rt sz”gek kisz mˇt sa - beforgat si sz”g‚rt‚kek }

{ X tengely k”rl szks‚ges forgat s sz mˇt sa, hogy Z = 0 legyen }
FUNCTION GetGX(P : K3D) : Real ;

{ Y tengely k”rl szks‚ges forgat s sz mˇt sa, hogy Z = 0 legyen }
FUNCTION GetGY(P: K3D) : Real ;

{ Z tengely k”rl szks‚ges forgat s sz mˇt sa, hogy Y = 0 legyen }
FUNCTION GetGZ(P : K3D) : Real ;

{ Dorn tetej‚nek "Anfang" koordin ta pontjainak sz mˇt sa }
PROCEDURE GetLAG( G1, G2 : K3D ;     { Els‹ ‚s a m sodik Geopont   }
                  LA : Real ;        { "A" =Dorn tetej‚nek hossza  }
                  VAR LAG : K3D) ;   { dorn tetej‚nek koord.       }

{ Dorn alj nak "Ende" koordin ta pontjainak sz mˇt sa }
PROCEDURE GetLEG( G1, G2 : K3D ;     { Utols˘ el‹tti ‚s utols˘ geopont }
                  LE : Real ;        { "E" =Dorn alj nak hossza        }
                  VAR LEG : K3D) ;   { dorn alj nak koord.             }

{ Hajlˇt si ˇvek / Biegeradien v‚gpont t vols g nak sz mˇt sa }
FUNCTION GetDL( G1, G2, G3 : K3D ;  { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }
                R : Real ) : Real ; { hajlˇt si sug r / Biegeradien }

{ Hajlˇt si ˇvek / Biegeradien v‚gpont t vols g nak sz mˇt sa t vols gokb˘l }
FUNCTION GetDLT( T1, T2, T3 : Real ;  { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }
                 R : Real ) : Real ; { hajlˇt si sug r / Biegeradien }

{ Hajlˇt si ˇvek / Biegeradien v‚gpont koordin t k sz mˇt sa }
PROCEDURE GetBiegePunkt( G1, G2, G3 : K3D ; { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }
                         R : Real ;         { hajlˇt si sug r / Biegeradien     }
                         VAR P21, P22 : K3D ) ; { ˇv v‚gpont koordin t k        }

{ maxim lis hajlˇt si sug r / Biegeradius kisz mˇt sa }
FUNCTION GetMaxR(P1, P2, P3 : K3D) : Real ; { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }

{ Túl kicsi adatok (< 0.001) adatok nullázása }
{ Zu kleine Daten wird auf 0.0 umgestellt     }
PROCEDURE DReset(VAR Data : Real) ;

{---------------------------------------------------------------}
{  X, Y, Z koordináták elforgatása tengelyek körül és eltolása  }
{---------------------------------------------------------------}
PROCEDURE ForgEltol( VAR X, Y, Z : Real ;   { Koordináták       }
                     Gx, Gy, Gz : Real ;    { Forgatási szögek  }
                     dX, dY, dZ : Real ) ;  { Eltolási értékek  }

{ Túl nagy ívek esetében, ha a túlhajlítási érték miatt 180 foknál nagyobb }
{ lenne a Biegewinkel, akkor kell 2 segéd Geopont, az ív kétfelé bontandó  }
{ és a hajlítási suagarakat csökkenteni kell 0,2 mm-el, hogy a két fél ív  }
{ között legyen 0,4 -0,6 mm távolság!                                      }
PROCEDURE GetNeueGeoPunkt( G1, G2, G3 : K3D ;  { Meglévő Geopontok }
                           R : Real ;       { Hajlítási sugár / Biegeradien }
                           VAR P2a, P2b : K3D ); { Új Geopontok / Neue Geopunkte }


{ 3 egymást követő Geopont standard beforgatási szögeinek kiszámmítása }
PROCEDURE GetOmega( G1, G2, G3 : K3D ;  { Geopontok }
                    VAR OmegaX, OmegaY, OmegaZ : Real) ;  // Beforgatási szögek

{ 2 Geopont /egyenes szakasz/ standard beforgatási szögeinek kiszámmítása }
PROCEDURE GetTheta( G1, G2 : K3D ;  { Geopontok }
                    VAR ThetaY, ThetaZ : Real) ;    // Beforgatási szögek


{  ***************************************  }
{                                           }
{               Ú j a k ! ! !               }
{                                           }
{  ***************************************  }

{ két 2D pont = szakasz átalakítása Vektorra }
FUNCTION Vect2D(Point1, Point2 : K2D) : K2D ;

{ két 3D pont = szakasz átalakítása Vektorra }
FUNCTION Vect3D(Point1, Point2 : K3D) : K3D ;

{ 3 térbeli pont = háromszög által meghatározott sík felület normálvektora }
function NormVector(Point1, Point2, Point3 : K3D) : K3D ;

{ 2 Vektor által közbezárt szög kiszámítása 2D-ben }
FUNCTION VectorAngle2D(Vect1, Vect2 : K2D): Real ;

{ 2 Vektor által közbezárt szög kiszámítása 3D-ben }
FUNCTION VectorAngle3D(Vect1, Vect2 : K3D): Real ;


{-----------------------------------------------------------------------------}
{*****************************************************************************}
{-----------------------------------------------------------------------------}

IMPLEMENTATION

{-----------------------------------------------------------------------------}
{*****************************************************************************}
{-----------------------------------------------------------------------------}

{--------------------------------------}
{  Szögfüggvények "fok" paraméterrel   }
{--------------------------------------}

FUNCTION FSIN(Alfa : Real) : Real ;
  BEGIN
    FSIN := Sin(Alfa * PI / 180.0) ;
  END ;

FUNCTION FCOS(Alfa : Real) : Real ;
  BEGIN
    FCOS := Cos(Alfa * PI / 180.0) ;
  END ;

FUNCTION FArcCos(E : Real) : Real ;
  BEGIN
    IF (E <= 1.0) AND (E >= -1.0) THEN BEGIN
      IF E = 0.0
        THEN FArcCos := 90.0
        ELSE FArcCos := -1.0 * Arctan(SQRT(1-SQR(E)) / E) * 180.0 / PI;
    END ;
  END ;

{  Matek képletek:
   ArcSin(x) = ArcTan (x/sqrt (1-sqr (x)))
   ArcCos(x) = ArcTan (sqrt (1-sqr (x)) /x)  }

FUNCTION FTAN(Alfa : Real) : Real ;
  BEGIN
    IF FCos(Alfa) = 0.0
      THEN FTAN := 1000000000000.0
      ELSE FTAN := FSin(Alfa) / FCos(Alfa) ;
  END ;

{ 2D-ben pont forgatás Origó körül Alfa szöggel (fok) }

PROCEDURE FORG2DO( Pbe : K2D ;
                   VAR Pki : K2D ;
                   Alfa : Real ) ;
  BEGIN
    Pki.X := (Pbe.X * FCOS(Alfa)) - (Pbe.Y * FSIN(Alfa)) ;
    Pki.Y := (Pbe.X * FSIN(Alfa)) + (Pbe.Y * FCOS(Alfa)) ;
  END ;


{ 2D-ben pont forgatás megadott "Pc" pont körül }

PROCEDURE FORG2D( Pc : K2D ;
                  Pbe : K2D ;
                  VAR Pki : K2D ;
                  Alfa : Real );
  VAR
    Ps : K2D ;
  BEGIN
    Ps.X := Pbe.X - Pc.X ;
    Ps.Y := Pbe.Y - Pc.Y ;

    FORG2DO(Ps, Pki, Alfa) ;

    Pki.X := Pki.X + Pc.X ;
    Pki.Y := Pki.Y + Pc.Y ;
  END ;

{ 3D-ben pont forgatása Origó körül }
PROCEDURE FORG3DO( Pbe : K3D ;
                   VAR Pki : K3D ;
                   Gx, Gy, Gz : Real ) ;
  VAR
    P2D, Ps : K2D ;
  BEGIN
    Ps.X := 0.0 ;
    Ps.Y := 0.0 ;
    { Forgat s X tengely k”rl }
    P2D.X := Pbe.Z ;
    P2D.Y := Pbe.Y ;
    FORG2DO(P2D, Ps, Gx) ;
    Pki.X := Pbe.X ;  { X ‚rt‚k nem v lzotik }
    Pki.Y := Ps.Y ;
    Pki.Z := Ps.X ;

    { Forgat s Y tengely k”rl }
    P2D.X := Pki.X ;
    P2D.Y := Pki.Z ;
    FORG2DO(P2D, Ps, Gy) ;
    { Pki.Y ‚rt‚k nem v lzotik }
    Pki.X := Ps.X ;
    Pki.Z := Ps.Y ;

    { Forgat s Z tengely k”rl }
    P2D.X := Pki.X ;
    P2D.Y := Pki.Y ;
    FORG2DO(P2D, Ps, Gz) ;
    { Pki.Z ‚rt‚k nem v lzotik }
    Pki.X := Ps.X ;
    Pki.Y := Ps.Y ;
  END ;


{ 3D-ben pont forgat sa Pc b zispont k”rl }
PROCEDURE FORG3D(  Pc : K3D ;
                   Pbe : K3D ;
                   VAR Pki : K3D ;
                   Gx, Gy, Gz : Real ) ;
  VAR
    Ps : K3D ;

  BEGIN
    Ps.X := Pbe.X - Pc.X ;
    Ps.Y := Pbe.Y - Pc.Y ;
    Ps.Z := Pbe.Z - Pc.Z ;

    FORG3DO(Ps, Pki, Gx, Gz, Gy) ;

    Pki.X := Pki.X + Pc.X ;
    Pki.Y := Pki.Y + Pc.Y ;
    Pki.Z := Pki.Z + Pc.Z ;
  END ;

{ ArcTan fggv‚ny amely "fok" ‚rt‚ked ad vissza }
FUNCTION FArcTan(E : Real) : Real ;
  BEGIN
    FArcTan := ArcTan(E) * 180.0 / PI ;
  END ;

{ 2D pont t vols g t adja vissza az orig˘t˘l }
FUNCTION Length2DO(P : K2D) : Real ;
  BEGIN
    Length2DO := SQRT(SQR(P.X) + SQR(P.Y)) ;
  END ;

{ k‚t 2D pont t vols g t adja vissza }
FUNCTION Length2D(P1, P2 : K2D) : Real ;
  BEGIN
    Length2D := SQRT(SQR(P2.X - P1.X) + SQR(P2.Y - P1.Y)) ;
  END ;

{ 3D pont t vols g t adja vissza az orig˘t˘l }
FUNCTION Length3DO(P : K3D) : Real ;
  BEGIN
    Length3DO := SQRT(SQR(P.X) + SQR(P.Y) + SQR(P.Z)) ;
  END ;

{ k‚t 3D pont t vols g t adja vissza }
FUNCTION Length3D(P1, P2 : K3D) : Real ;
  BEGIN
    Length3D := SQRT(SQR(P2.X - P1.X) + SQR(P2.Y - P1.Y) + SQR(P2.Z - P1.Z)) ;
  END ;

{ két egyenes "A"és "B" által bezárt szög kiszámítása = inv. Cosinus tétel }
{ Gamma a "C" oldallal szembeni szög }
FUNCTION GetGamma(A, B, C : Real) : Real ;
  VAR
    E : Real ;
  BEGIN
    IF C >= (A+B)                { rossz adat vizsg lat }
      THEN GetGamma := 180.0
      ELSE
        BEGIN
          E := ABS(FArcCos((SQR(A) + SQR(B) - SQR(C))/(2.0 * A * B))) ;
          IF C < SQRT(SQR(A) + SQR(B)) THEN E := 180.0 - E ;
          GetGamma := E ;
        END ;
  END ;

{ 3D-ben két egymást követő 3 egyenes által bezárt szög számítása
  = harmadik oldallal szembeni szög  = Biegewinkel }
FUNCTION GetBiegewinkel(P1, P2, P3 : K3D) : Real ;
  VAR
    L1, L2, L3 : Real ;  { sarokpontok k”z”tti t vols g }
  BEGIN
    L1 := Length3D(P1, P2) ;
    L2 := Length3D(P2, P3) ;
    L3 := Length3D(P1, P3) ;
    GetBiegewinkel := 180.0 - GetGamma(L1, L2, L3) ;
  END ;

{ 2D koordináták átalakítása polárkoordinátákká, Origóhoz képest }
PROCEDURE GetPolar2DO( P : K2D ;
                       VAR Pol: P2D) ;
  VAR
    Szog : Real ;

  BEGIN
    Pol.L := Length2DO(P) ;
    Pol.Gamma := 0.0 ;
    Szog := 0.0 ;

    IF (P.X = 0.0) AND (P.Y = 0.0)
      THEN Szog := 0.0
      ELSE
        IF P.X = 0.0
          THEN BEGIN
              IF P.Y > 0.0 THEN Szog := 90.0
              ELSE Szog := 270.0
            END
          ELSE
            IF P.Y = 0.0
            THEN BEGIN
                IF P.X > 0.0 THEN Szog := 0.0
                ELSE Szog := 180.0
              END
            ELSE BEGIN
              Szog := FArcTan(P.Y / P.X) ;
              IF (P.X < 0.0) THEN Szog := Szog + 180.0 ;
            END ;
    Pol.Gamma := Szog ;
  END ;

{ 2D koordináták átalakítása polárkoordinátákká, Pc bázisponthoz képest }
PROCEDURE GetPolar2D( Pc : K2D ;
                      P : K2D ;
                      VAR Pol: P2D) ;
  VAR
    Ps : K2D ;
  BEGIN
    Ps.X := P.X - Pc.X ;
    Ps.Y := P.Y - Pc.Y ;
    GetPolar2DO(Ps, Pol) ;
  END ;


{ 3D koordináták átalakítása polárkoordinátákká origóhoz képest }
PROCEDURE GetPolar3DO( P : K3D ;
                       VAR Pol : P3D ) ;
  VAR
    Ps1, Ps2 : K2D ;
    PP : P2D ;
  BEGIN
    PP.Gamma := 0.0 ;
    PP.L := 0.0 ;

    Ps1.X := P.X ;
    Ps1.Y := P.Y ;

    GetPolar2DO(Ps1, PP) ;

    Pol.Gamma := PP.Gamma ;  { XY vetltnek az X tengellyel bez rt sz”ge }

    Ps2.X := SQRT(SQR(P.X) + SQR(P.Y)) ;
    Ps2.Y := P.Z ;

    GetPolar2DO(Ps2, PP) ;

    Pol.Theta := PP.Gamma ;  { vektornak az XY sˇkkal bez rt sz”ge }
    Pol.L := Length3DO(P) ;  { vektor hossza                       }
  END ;

{ 3D koordináták átalakítása polárkoordinátákká, Pc bázisponthoz képest }
PROCEDURE GetPolar3D( Pc : K3D ;          { b zispont                   }
                      P : K3D ;           { Geopont                     }
                      VAR Pol : P3D ) ;   { sz mˇtott pol r koordin t k }
  VAR
    Ps : K3D ;
  BEGIN
    Ps.X := P.X - Pc.X ;
    Ps.Y := P.Y - Pc.Y ;
    Ps.Z := P.Z - Pc.Z ;
    GetPolar3DO(Ps, Pol) ;
  END ;

{ Origóban lévő polárkoordináta átalakítása 3D-koordinátákká }
PROCEDURE GetKoord3DO( PP : P3D ;
                       VAR PK : K3D ) ;
  BEGIN
    PK.X := PP.L * FCos(PP.Theta) * FCos(PP.Gamma) ;
    PK.Y := PP.L * FCos(PP.Theta) * FSin(PP.Gamma) ;
    PK.Z := PP.L * FSin(PP.Theta) ;
  END ;


{ PK ponthoz képest PP polárkoordináta átalakítása 3D-koordinátákká }
PROCEDURE GetKoord3D( PK : K3D ;
                      PP : P3D ;
                      VAR PK2 : K3D ) ;
  VAR
    Ps : K3D ;
  BEGIN
    Ps.X := 0.0 ;
    Ps.Y := 0.0 ;
    Ps.Z := 0.0 ;
    GetKoord3DO(PP, Ps) ;
    PK2.X := Ps.X + PK.X ;
    PK2.Y := Ps.Y + PK.Y ;
    PK2.Z := Ps.Z + PK.Z ;
  END ;


{ Síknegyedekkel bezárt szögek kiszámítása = beforgatási szög értékek }

{ X tengely körül szükséges forgatás számítása, hogy Z = 0 legyen }
FUNCTION GetGX(P : K3D) : Real ;
  VAR
    Ps : K2D ;
    PP : P2D ;
  BEGIN
    PP.Gamma := 0.0 ;
    PP.L := 0.0 ;

    Ps.X := P.Y ;
    Ps.Y := P.Z ;

    GetPolar2DO(Ps, PP) ;
    GetGX := PP.Gamma ;
  END ;

{ Y tengely körül szükséges forgatás számítása, hogy Z = 0 legyen }
FUNCTION GetGY(P: K3D) : Real ;
  VAR
    Ps : K2D ;
    PP : P2D ;
  BEGIN
    PP.Gamma := 0.0 ;
    PP.L := 0.0 ;

    Ps.X := P.X ;
    Ps.Y := P.Z ;

    GetPolar2DO(Ps, PP) ;
    GetGY := PP.Gamma ;
  END ;

{ Z tengely körül szükséges forgatás számítása, hogy Y = 0 legyen }
FUNCTION GetGZ(P : K3D) : Real ;
  VAR
    Ps : K2D ;
    PP : P2D ;
  BEGIN
    PP.Gamma := 0.0 ;
    PP.L := 0.0 ;

    Ps.X := P.X ;
    Ps.Y := P.Y ;

    GetPolar2DO(Ps, PP) ;
    GetGZ := PP.Gamma ;
  END ;

{ Dorn tetej‚nek "Anfang" koordin ta pontjainak sz mˇt sa }
PROCEDURE GetLAG( G1, G2 : K3D ;     { Els‹ ‚s a m sodik Geopont   }
                  LA : Real ;        { "A" =Dorn tetej‚nek hossza  }
                  VAR LAG : K3D) ;   { dorn tetej‚nek koord.       }
  VAR
    GP : P3D ;
  BEGIN
    GP.Gamma := 0.0 ;
    GP.Theta := 0.0 ;
    GP.L     := 0.0 ;
    GetPolar3D(G2, G1, GP) ;
    GP.L := LA ;
    GetKoord3D(G1, GP, LAG) ;
  END ;

{ Dorn alj nak "Ende" koordin ta pontjainak sz mˇt sa }
PROCEDURE GetLEG( G1, G2 : K3D ;     { Utols˘ el‹tti ‚s utols˘ geopont }
                  LE : Real ;        { "E" =Dorn alj nak hossza        }
                  VAR LEG : K3D) ;   { dorn alj nak koord.             }
  VAR
    GP : P3D ;
  BEGIN
    GP.Gamma := 0.0 ;
    GP.Theta := 0.0 ;
    GP.L     := 0.0 ;
    GetPolar3D(G1, G2, GP) ;
    GP.L := LE ;
    GetKoord3D(G2, GP, LEG) ;
  END ;

{ Hajlítási ívek / Biegeradien végpont távolságának számítása }
FUNCTION GetDL( G1, G2, G3 : K3D ;  { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }
                R : Real ) : Real ; { hajlˇt si sug r / Biegeradien }
  BEGIN
    GetDL := R * FTAN(180.0 - GetGamma( Length3D(G1, G2),
                                        Length3D(G2, G3),
                                        Length3D(G1, G3)) * 0.5) ;
  END ;

{ Túl nagy ívek esetében, ha a túlhajlítási érték miatt 180 foknál nagyobb }
{ lenne a Biegewinkel, akkor kell 2 segéd Geopont, az ív kétfelé bontandó  }
{ és a hajlítási suagarakat csökkenteni kell 0,2 mm-el, hogy a két fél ív  }
{ között legyen 0,4 ... 0,6 mm távolság!                                      }

PROCEDURE GetNeueGeoPunkt( G1, G2, G3 : K3D ;  { Meglévő Geopontok }
                           R : Real ;       { Hajlítási sugár / Biegeradien }
                           VAR P2a, P2b : K3D ); { Új Geopontok / Neue Geopunkte }
  VAR
    A, C : Real ;
    { A : G2 és ív középpont távolsága }
    { C : G2 és P2a pontok távolsága = G2 és P2b pont távolsága  }
    Gamma : Real ;  { 180 - Biegewinkel = G2 pontnál a közbezárt szög }
    GP : P3D ;  { Egyenes szakasz polárkoordinátái / irányvektor }
  BEGIN
    Gamma := 180.0 - GetGamma( Length3D(G1, G2),
                               Length3D(G2, G3),
                               Length3D(G1, G3)) ;

    //MessageDlg(Format('%.2f', [Gamma]), mtInformation, [mbOk], 0, mbOk) ;

    A := R / FSIN(Gamma / 2.0) ;

    C := (A - R) / FCOS(Gamma / 2.0) ;

    //MessageDlg(Format('%.2f', [R]), mtInformation, [mbOk], 0, mbOk) ;
    //MessageDlg(Format('%.2f', [A]), mtInformation, [mbOk], 0, mbOk) ;
    //MessageDlg(Format('%.2f', [C]), mtInformation, [mbOk], 0, mbOk) ;

    GP.Gamma := 0.0 ;
    GP.Theta := 0.0 ;
    GP.L     := 0.0 ;
    GetPolar3D(G2, G1, GP) ;   // G2 >>> G1 Irányvektor számítás
    GP.L := C ;                // P2a pont távolsága a sarokponttól
    GetKoord3D(G2, GP, P2a) ;  // P2a koordináták kiszámítása

    GP.Gamma := 0.0 ;
    GP.Theta := 0.0 ;
    GP.L     := 0.0 ;
    GetPolar3D(G2, G3, GP) ;   // G2 >>> G3 Irányvektor számítás
    GP.L := C ;                // P2b pont távolsága a sarokponttól
    GetKoord3D(G2, GP, P2b) ;  // P2b koordináták kiszámítása
  END ;

{ Hajlítási ívek / Biegeradien végpont távolságának számítása távolságokból }
FUNCTION GetDLT( T1, T2, T3 : Real ;  { Geopontok, középső ívnél számítása }
                 R : Real ) : Real ;  { hajlítási sugár / Biegeradien }
  BEGIN
    GetDLT := R * FTAN(180.0 - GetGamma(T1, T2, T3) * 0.5) ;
  END ;

{ Hajlˇt si ˇvek / Biegeradien v‚gpont koordin t k sz mˇt sa }
PROCEDURE GetBiegePunkt( G1, G2, G3 : K3D ; { Geopontok, k”z‚ps‹ ˇvn‚l sz mˇt s }
                         R : Real ;         { hajlˇt si sug r / Biegeradien     }
                         VAR P21, P22 : K3D ) ; { ˇv v‚gpont koordin t k        }
  VAR
    GP : P3D ;  { Egyenes szakasz pol rkoordin t i / ir nyvektor }
    DL : Real ;
  BEGIN
    GP.Gamma := 0.0 ;
    GP.Theta := 0.0 ;
    GP.L     := 0.0 ;

    DL := ABS(GetDL(G1, G2, G3, R)) ;  // ív kezdő pont és sarokpont távolsága

    GetPolar3D(G2, G1, GP) ;   // G2 >>> G1 Irányvektor számítás
    GP.L := DL ;               // P21 pont távolsága a sarokponttól
    GetKoord3D(G2, GP, P21) ;  // P21 koordináták kiszámítása
    {------------------------------------------------------------------------}
    GetPolar3D(G2, G3, GP) ;   // G2 >>> G3 Irányvektor számítás
    GP.L := DL ;               // P22 pont távolsága a sarokponttól
    GetKoord3D(G2, GP, P22) ;  // P22 koordináták kiszámítása

    { megoldás szakaszok aránypárral:
    VAR
      S : K3D ;
      dL : Real ;
      T : Real ;
    BEGIN
      dL := ABS(GetDL(G1, G2, G3, R)) ;

      S.X := G1.X - G2.X ;
      S.Y := G1.Y - G2.Y ;
      S.Z := G1.Z - G2.Z ;
      T := SQRT(SQR(S.X) + SQR(S.Y) + SQR(S.Z)) ;
      IF G1 = G2
        THEN P21 := G2
        ELSE
          BEGIN
            P21.X := G2.X - (S.X * dL / T) ;
            P21.Y := G2.Y - (S.X * dL / T) ;
            P21.Z := G2.Z - (S.X * dL / T) ;
          END ;

      S.X := G3.X - G2.X ;
      S.Y := G3.Y - G2.Y ;
      S.Z := G3.Z - G2.Z ;
      T := SQRT(SQR(S.X) + SQR(S.Y) + SQR(S.Z)) ;
      IF G3 = G2
        THEN P21 := G2
        ELSE
          BEGIN
            P22.X := G2.X + (S.X * dL / T) ;
            P22.Y := G2.Y + (S.X * dL / T) ;
            P22.Z := G2.Z + (S.X * dL / T) ;
          END ;
    END ; }

  END ;

{ maximális hajlítási sugár / Biegeradius kiszámítása       }
{ P1, P2, P3 : Geopontok, középső ívnél max. sugár számítás }
FUNCTION GetMaxR(P1, P2, P3 : K3D) : Real ;
  VAR
    Lmin       : Real ;  { Rövidebbik oldal             }
    L1, L2, L3 : Real ;  { Sarokpontok közötti távolság }
    Gamma      : Real ;  { közbezárt szög               }
  BEGIN
    L1 := Length3D(P1, P2) ;
    L2 := Length3D(P2, P3) ;
    L3 := Length3D(P1, P3) ;
    Gamma := GetGamma(L1, L2, L3) ;
    IF L1 < L2 THEN Lmin := L1 ELSE Lmin := L2 ;

    { Ha a közbezárt szög nulla, akkor a két egyenes párhuzamos }
    IF (Gamma = 0.0) OR (Gamma = 180.0)
      THEN GetMaxR := 0.0
      ELSE GetMaxR := Lmin / FTan(Gamma * 0.5) ;
  END ;


{-----------------------------------}
{  Pont síkban elforgatás képletei  }
{-----------------------------------}
FUNCTION GetT1(T1, T2, Gamma : Real) : Real ;
BEGIN
  GetT1 := (T1 * Cos(Gamma * PI / 180.0)) - (T2 * Sin(Gamma * PI / 180.0)) ;
END ;

FUNCTION GetT2(T1, T2, Gamma : Real) : Real ;
BEGIN
  GetT2 := (T1 * Sin(Gamma * PI / 180.0)) + (T2 * Cos(Gamma * PI / 180.0)) ;
END ;

{---------------------------------------------------------------}
{  X, Y, Z koordináták elforgatása tengelyek körül és eltolása  }
{---------------------------------------------------------------}
PROCEDURE ForgEltol( VAR X, Y, Z : Real ;   { Koordináták       }
                     Gx, Gy, Gz : Real ;    { Forgatási szögek  }
                     dX, dY, dZ : Real ) ;  { Eltolási értékek  }
VAR
  Xu1, Yu1, Zu1 : Real ;
  Xu2, Yu2, Zu2 : Real ;
  Xu3, Yu3, Zu3 : Real ;
BEGIN
  { X tengely körül forgatunk }
  Xu1 := X ; { nem változik }
  Yu1 := GetT1(Y, Z, Gx) ;
  Zu1 := GetT2(Y, Z, Gx) ;

  { Y tengely körül forgatunk }
  Xu2 := GetT1(Xu1, Zu1, Gy) ;
  Yu2 := Yu1 ; { nem változik }
  Zu2 := GetT2(Xu1, Zu1, Gy) ;

  { Z tengely körül forgatunk }
  Xu3 := GetT1(Xu2, Yu2, Gz) ;
  Yu3 := GetT2(Xu2, Yu2, Gz) ;
  Zu3 := Zu2 ; { nem változik }

  X := Xu3 + dX ;   { X koordinátákat eltoljuk }
  Y := Yu3 + dY ;   { Y koordinátákat eltoljuk }
  Z := Zu3 + dZ ;   { Z koordinátákat eltoljuk }
END ;


{ Túl kicsi adatok (< 0.001) adatok nullázása }
{ Zu kleine Daten wird auf 0.0 umgestellt     }
PROCEDURE DReset(VAR Data : Real) ;
  BEGIN
    IF (Data < 0.001) AND (Data > -0.001) THEN Data := 0.0 ;
  END ;


{------------------------------------------------------------------------------}
{  NET-ről letöltött !!!   Radiánban vannak a szögek !!!                       }
{------------------------------------------------------------------------------}

FUNCTION ARCSINR(X : Real) : Real; (* arc sine using TP arctan function *)
 BEGIN                                 (* answer returned in radians *)
   IF X = 1.0 THEN
     ARCSINR := Pi / 2.0
   ELSE
     IF X = - 1.0 THEN
        ARCSINR := Pi / - 2.0
     ELSE
       ARCSINR := Arctan(X / Sqrt(1.0 - Sqr(X)))
 END;                                  (* ARCSIN *)

FUNCTION ARCCOSR(X : Real) : Real; (* inverse cosine using TP arctan *)
 BEGIN                                 (* answer returned in radians *)
   IF X = 0.0 THEN    ARCCOSR := Pi / 2.0  ELSE
     IF X < 0.0 THEN
       ARCCOSR := Pi - Arctan(Sqrt(1.0 - Sqr(X)) / Abs(X))
     ELSE
       ARCCOSR := Arctan(Sqrt(1.0 - Sqr(X)) / Abs(X))
 END;                                  (* ARCCOS *)

{ 3 egymást követő Geopont standard beforgatási szögeinek kiszámmítása }
PROCEDURE GetOmega( G1, G2, G3 : K3D ;  { Geopontok }
                    VAR OmegaX, OmegaY, OmegaZ : Real) ; // Beforgatási szögek
  BEGIN
    { Nullázás }
    OmegaX := 0.0 ;    OmegaY := 0.0 ;    OmegaZ := 0.0 ;

    { Geometria pontok eltolása, G1 pont origóba }
    G2.X := G2.X - G1.X ;    G2.Y := G2.Y - G1.Y ;    G2.Z := G2.Z - G1.Z ;
    G3.X := G3.X - G1.X ;    G3.Y := G3.Y - G1.Y ;    G3.Z := G3.Z - G1.Z ;

    { G1 pont origóba }
    G1.X := 0.0 ;    G1.Y := 0.0 ;    G1.Z := 0.0 ;

    { G1 pont már az origóban van, ezért a G2 pont }
    { koordinátáiból kiszámítjuk a szükséges Z (Gz) és Y (Gy) }
    { tengely körüli szögelforgatási értékeket                }

    { Z tengely körüli forgatás értéke, hogy 2. Geopont Y = 0 legyen }
    OmegaZ := GetGz(G2) ;

    { Elforgatjuk G2 + G3 geopontot a Z tengely körül }
    Forg3DO(G2, G2, 0, 0, -OmegaZ) ;
    Forg3DO(G3, G3, 0, 0, -OmegaZ) ;

    { Y tengely körüli forgatás értéke, hogy Z = 0 legyen }
    OmegaY := GetGy(G2) ;

    { Elforgatjuk a G2 + G3 geopontot az Y tengely körül }
    Forg3DO(G2, G2, 0, -OmegaY, 0) ;
    Forg3DO(G3, G3, 0, -OmegaY, 0) ;

    { A G1 = origóban van, a G2 pont már az X tengelyen }
    { A G3 geopont koordinátáiból számítjuk az }
    { X tengely körüli elfogatási értéket      }
    OmegaX := GetGx(G3) ;
  END ;

{ 2 Geopont /egyenes szakasz/ standard beforgatási szögeinek kiszámmítása }
PROCEDURE GetTheta( G1, G2 : K3D ;  { Geopontok }
                    VAR ThetaY, ThetaZ : Real) ;    // Beforgatási szögek

  BEGIN
    { Nullázás }
    ThetaY := 0.0 ;    ThetaZ := 0.0 ;

    { Geometria pontok eltolása, G1 pont origóba }
    G2.X := G2.X - G1.X ;    G2.Y := G2.Y - G1.Y ;    G2.Z := G2.Z - G1.Z ;

    { G1 pont origóba }
    G1.X := 0.0 ;    G1.Y := 0.0 ;    G1.Z := 0.0 ;

    { G1 pont már az origóban van, ezért a G2 pont }
    { koordinátáiból kiszámítjuk a szükséges Z (Gz) és Y (Gy) }
    { tengely körüli szögelforgatási értékeket                }

    { Z tengely körüli forgatás értéke, hogy 2. Geopont Y = 0 legyen }
    ThetaZ := GetGz(G2) ;

    { Elforgatjuk G2 geopontot a Z tengely körül }
    Forg3DO(G2, G2, 0, 0, -ThetaZ) ;

    { Y tengely körüli forgatás értéke, hogy Z = 0 legyen }
    ThetaY := GetGy(G2) ;
  END ;


{  ***************************************  }
{                                           }
{               Ú j a k ! ! !               }
{                                           }
{  ***************************************  }


{ két 2D pont = szakasz átalakítása Vektorra }
FUNCTION Vect2D(Point1, Point2 : K2D) : K2D ;
  BEGIN
    Result.X := Point2.X - Point1.X ;
    Result.Y := Point2.Y - Point1.Y ;
  END ;

{ két 3D pont = szakasz átalakítása Vektorra }
FUNCTION Vect3D(Point1, Point2 : K3D) : K3D ;
  BEGIN
    Result.X := Point2.X - Point1.X ;
    Result.Y := Point2.Y - Point1.Y ;
    Result.Z := Point2.Z - Point1.Z ;
  END ;

{ 3 térbeli pont = háromszög által meghatározott sík felület normálvektora }
{ Forrás. ChatOpenAI     szükséges a korrekt STL file adataihoz            }
{   Ez a függvény három pontot vár bemenetként (TPoint3D típusú), és
    visszaadja a sík normálvektorát (szintén TPoint3D típusúként).
    Megjegyzem, hogy ez az algoritmus feltételezi, hogy a három pont nem
    egy egyenesen helyezkedik el, és nem van olyan eset,
    amikor a három pont azonos. }

FUNCTION NormVector(Point1, Point2, Point3 : K3D) : K3D ;
  VAR
    Vector1, Vector2: K3D ;
  BEGIN
    // Első vektor számítása
    Vector1.X := Point2.X - Point1.X ;
    Vector1.Y := Point2.Y - Point1.Y ;
    Vector1.Z := Point2.Z - Point1.Z ;

    // Második vektor számítása
    Vector2.X := Point3.X - Point1.X ;
    Vector2.Y := Point3.Y - Point1.Y ;
    Vector2.Z := Point3.Z - Point1.Z ;

    // Normálvektor kiszámítása a keresztszorzat segítségével
    Result.X := Vector1.Y * Vector2.Z - Vector1.Z * Vector2.Y ;
    Result.Y := Vector1.Z * Vector2.X - Vector1.X * Vector2.Z ;
    Result.Z := Vector1.X * Vector2.Y - Vector1.Y * Vector2.X ;
  END ;


{ 2 Vektor által közbezárt szög kiszámítása 2D-ben }
FUNCTION VectorAngle2D(Vect1, Vect2 : K2D): Real ;
  VAR
    DotProd, Len1, Len2 : Real ;
  BEGIN
    DotProd := (Vect1.X * Vect2.X) + (Vect1.Y * Vect2.Y) ;
    // Len1 := SQRT(Vect1.X * Vect1.X + Vect1.Y * Vect1.Y) ;
    // Len2 := SQRT(Vect2.X * Vect2.X + Vect2.Y * Vect2.Y) ;
    Len1 := SQRT(SQR(Vect1.X) + SQR(Vect1.Y)) ;
    Len2 := SQRT(SQR(Vect2.X) + SQR(Vect2.Y)) ;
    Result := FArcCos(DotProd / (Len1 * Len2)) ;
  END ;

{ 2 Vektor által közbezárt szög kiszámítása 3D-ben }
FUNCTION VectorAngle3D(Vect1, Vect2 : K3D): Real ;
  VAR
    DotProd, Len1, Len2 : Real ;
  BEGIN
    DotProd := (Vect1.X * Vect2.X) + (Vect1.Y * Vect2.Y) + (Vect1.Z * Vect2.Z) ;
    // Len1 := SQRT(Vect1.X * Vect1.X + Vect1.Y * Vect1.Y + Vect1.Z * Vect1.Z) ;
    // Len2 := SQRT(Vect2.X * Vect2.X + Vect2.Y * Vect2.Y + Vect2.Z * Vect2.Z) ;
    Len1 := SQRT(SQR(Vect1.X) + SQR(Vect1.Y) + SQR(Vect1.Z)) ;
    Len2 := SQRT(SQR(Vect2.X) + SQR(Vect2.Y) + SQR(Vect2.Z)) ;
    Result := FArcCos(DotProd / (Len1 * Len2)) ;
  END ;

{ 3D Vektorok kereszt szorzata  = normál Vektor kalkuláció }
FUNCTION VectorCross(Vector1, Vector2 : K3D) : K3D ;
  BEGIN
    Result.X := Vector1.Y * Vector2.Z - Vector1.Z * Vector2.Y ;
    Result.Y := Vector1.Z * Vector2.X - Vector1.X * Vector2.Z ;
    Result.Z := Vector1.X * Vector2.Y - Vector1.Y * Vector2.X ;
  END ;

{ 2D Vektor hossza }
FUNCTION LenVect2D(Vector : K2D) : Real ;
  BEGIN
    Result := SQRT(SQR(Vector.X) + SQR(Vector.Y)) ;
  END ;

{ 3D Vektor hossza }
FUNCTION LenVect(Vector : K3D) : Real ;
  BEGIN
    Result := SQRT(SQR(Vector.X) + SQR(Vector.Y) + SQR(Vector.Z)) ;
  END ;


{ GetBiegewinkel számítása Vektorokkal }
{ 3D-ben két egymást követő egyenes szakasz által bezárt szög számítása
  = harmadik oldallal szembeni szög = Biegewinkel }
FUNCTION GetBiegewinkelVect(P1, P2, P3 : K3D) : Real ;
  VAR
    Vect1  : K3D ;               // P1 -> P2 Vektor
    Vect2  : K3D ;               // P1 -> P3 Vektor
    Gamma  : Real ;              // Biegewinkel
  BEGIN
    Vect1 := Vect3D(P1, P2) ;    // P1 -> P2 Vektor
    Vect2 := Vect3D(P1, P3) ;    // P1 -> P3 Vektor
    Result := 180.0 - VectorAngle3D(Vect1, Vect2) ;
  END ;

{ GetDrehwinkel számítása Vektorokkal }
{ 3D-ben három egymást követő 3 egyenes szakasz által bezárt szög számítása
  = harmadik oldallal szembeni szög = Biegewinkel }
FUNCTION GetDrehwinkelVect(P1, P2, P3, P4 : K3D) : Real ;
  VAR
    Vect12, Vect23, Vect34 : K3D ;
    NVEctA, NVectB         : K3D ;
  BEGIN
    Vect12 := Vect3D(P1, P2) ;
    Vect23 := Vect3D(P2, P3) ;
    Vect34 := Vect3D(P3, P4) ;
    NVectA := VectorCross(Vect12, Vect23) ;  // ki kell próbálni!!!!
    NVectB := VectorCross(Vect12, Vect23) ;  // ki kell próbálni!!!!
    Result := VectorAngle3D(NVectA, NVectB) ;  // ki kell próbálni!!!!
  END ;

{----------------------------------------------------------------------------}
{ XYZ >>> YBC (LRA) Length, Rotation, Angle = Lange, Drehwinkel, Biegewinkel }
{----------------------------------------------------------------------------}








BEGIN
END.
