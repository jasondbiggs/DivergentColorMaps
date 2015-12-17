BeginPackage["DivergentColorMaps`"]

DivergentColorFunc::usage = "DivergentColorFunc[{r1,g1,b1},{r2,b2,g2}] returns a continuously diverging color map which interpolates between two RGB colors.\n
DivergentColorFunc[color1, color2] takes two color objecst as input and returns a continuously diverging color map."
CoolToWarm::usage = "Cool2Warm[n] gives the cool to warm color map, with n taking values between 0 and 1"
DivergentColorScheme::usage = "DivergentColorScheme[scheme] gives a diverging color map which interpolates between the starting and ending colors in a builtin scheme"
DivergentMaps::usage = "DivergentMaps is list of four divergent color maps used in http://www.kennethmoreland.com/color-maps/ColorMapsExpanded.pdf . divergentMaps[[1]] is equivalent to Cool2Warm" 

Begin["`Private`"]



(* 
The reference white values and transformation matrix correspond to the 
fact that in Mathematica, the RGB white point uses the D65 standard, 
while the XYZ and LAB color spaces use the D50 white point.  This is 
different than in Moreland's paper or other color conversion websites 
*)

referenceWhite = {96.42, 100.0, 82.49};


transformation = {{0.436075, 0.385065, 0.14308}, 
   {0.222504, 0.716879, 0.0606169},
   {0.0139322, 0.0971045, 0.714173}};
   
(*Forward Transformations*)

rgb2xyz[r_, g_, b_] := Module[
   {transm, rl, gl, bl},
   {rl, gl, bl} = If[# > .04045,
       ((# + 0.055)/1.055)^2.4,
       #/12.92] & /@ {r, g, b};
   transm = transformation;
   100 transm.{rl, gl, bl}
   ];

xyz2lab[xi_, yi_, zi_] := Module[{f, refx, refy, refz, x, y, z},
   {refx, refy, refz} = referenceWhite;
   f = If[((#) > 0.008856),
      (#^(1/3)),
      (7.787 # + 4/29.)] &;
   {x, y, z} = f /@ ({xi, yi, zi}/{refx, refy, refz});
   {116.0 (y - 4./29), 500.0 (x - y), 200 (y - z)}
   ];

lab2msh[l_, a_, b_] := Module[{m = Norm[{l, a, b}]}, {m, 
   If[m==0, 0, ArcCos[l/m]], Arg[a + b I]}];
rgb2msh[r_, g_, b_] := lab2msh @@ xyz2lab @@ rgb2xyz @@ {r, g, b};

(* Backward Transformations *)

msh2lab[m_, s_, h_] := {m Cos[s], m Sin[s] Cos[h], m Sin[s] Sin[h]};

lab2xyz[l_, a_, b_] := Module[{x, y, z, refx, refy, refz},
   {refx, refy, refz} = referenceWhite;
   y = (l + 16)/116.;
   x = a/500. + y;
   z = y - b/200.;
   {x, y, z} = 
    If[#^3 > 0.008856, #^3, (# - 4./29)/7.787] & /@ {x, y, z};
   {x, y, z} {refx, refy, refz}
   ];

xyz2rgb[x_, y_, z_] := Module[{transm, r, g, b},
   transm = Inverse@transformation;
   {r, g, b} = {x, y, z}/100;
   {r, g, b} = transm.{r, g, b};
   If[# > 0.0031308, 1.055 #^(1/2.4) - 0.055, 12.92 #] & /@ {r, g, b}
   ];

msh2rgb[m_, s_, h_] := xyz2rgb @@ lab2xyz @@ msh2lab @@ {m, s, h};

adjusthue[msat_, ssat_, hsat_, munsat_] := Module[{hspin},
   If[msat >= munsat,
    hsat,
    hspin = ssat Sqrt[munsat^2 - msat^2]/(msat Sin[ssat]);
    If[hsat > -\[Pi]/3,
     hsat + hspin,
     hsat - hspin
     ]
    ]
   ];
   
 
interpolatecolor[rgb1_List, rgb2_List, interp_?NumericQ] := 
  Module[
   {m1, s1, h1, m2, s2, h2, interpvar, mmid, smid, hmid},
   (*If points are saturated and distinct, 
   place white in the middle *)
   {m1, s1, h1} = 
    rgb2msh @@ rgb1;
   {m2, s2, h2} = rgb2msh @@ rgb2;
   interpvar = interp;
   If[s1 > 0.05 && s2 > 0.05 && Abs[h1 - h2] > Pi/3,
    mmid = Max@{m1, m2, 88.};
    If[interp < 1/2,
     {m2, s2, h2, interpvar} = {mmid, 0, 0, 2 interp};,
     {m1, s1, h1, interpvar} = {mmid, 0, 0, 2 interp - 1};
     ];
    ];
   (* Adjust hue of unsaturated colors *)
   
   Which[s1 < 0.05 && s2 > 0.05,
    h1 = adjusthue[m2, s2, h2, m1];,
    s2 < 0.05 && s1 > 0.05,
    h2 = adjusthue[m1, s1, h1, m2];
    ];
   {mmid, smid, hmid} = (1 - interpvar) {m1, s1, h1} + 
     interpvar {m2, s2, h2};
   msh2rgb @@ {mmid, smid, hmid}
   
   ];

DivergentColorFunc[rgb1_, rgb2_] := 
  With[{interp = RGBColor @@@ Chop @ (interpolatecolor[rgb1, rgb2, #] &/@ Range[0,1,.05])},
      Blend[interp, #] & ];

   
DivergentColorFunc[col1_?ColorQ, col2_?ColorQ] := DivergentColorFunc @@ List @@@ (ColorConvert[{col1, col2}, RGBColor]) ;

DivergentColorScheme[scheme_String] := 
  DivergentColorFunc @@ ColorData[scheme] /@ {0, 1};




CoolToWarm = DivergentColorFunc[{0.23, 0.299, 0.754}, {0.706, 0.016, 0.150}];

  
DivergentMaps = 
  DivergentColorFunc[#1, #2] & @@@ {{{0.23, 0.299, 0.754}, {0.706, 
      0.016, 0.150}},
    {{0.436, 0.308, 0.631}, {0.759, 0.334, 0.046}}, {{0.085, 0.532, 
      0.201}, {0.436, 0.308, 0.631}}, {{0.217, 0.525, 0.910}, {0.677, 
      0.492, 0.093}}, {{0.085, 0.532, 0.201}, {0.758, 0.214, 0.233}}};

End[]
EndPackage[]
