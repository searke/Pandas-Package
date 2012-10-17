(* ::Package:: *)

BeginPackage["Pandas`"]

PandasSeries::usage=
	"A data structure that behaves similarly to a series
	data structure in the popular Python Pandas package."

Index::usage= 
	"Gives the index of one of these series"

Data::usage=
	"Returns the acutal data of the series"

Name::usage=
	"Gives the name of the series"

Begin["Private`"]

(*Type pandasSeries x pandasSeries -> pandasSeries?*)
pandasSeries/:
Part[ps_pandasSeries,boolps_pandasSeries/;VectorQ[Data@boolps,BooleQ]]:=
	pandasSeries[Pick[Data@ps,Data@boolps],
				If[Index@ps=!=Null,Pick[Index@ps,Data@boolps]],
				Name@ps];

(*Type pandasSeries x List -> pandasSeries*)
pandasSeries/:
Part[ps_pandasSeries,boolList_/;VectorQ[boolList,BooleQ]]:=
	pandasSeries[Pick[Data@ps,boolList],
				If[Index@ps=!=Null,Pick[Index@ps,boolList]],
				Name@ps];

(*Type pandasSeries x int|span -> pandasSeries*)
pandasSeries/:
Part[ps_pandasSeries,int_Integer|int_Span]:=
	pandasSeries[Data[ps][[int]],
				If[Index@ps=!=Null,Index[ps][[int]]],
				Name@ps];

(*Type pandasSeries x (_->Boole) -> pandasSeries *)
pandasSeries/:
Part[ps_pandasSeries,func_]:=
(* If ps has an Index, transpose and select both the indexes and data*)
	If[Index@ps=!=Null,
		With[{selection=Select[Transpose[{Data@ps,Index@ps}],func@First@#&]},
		Transpose@selection/.{data_,index_}->pandasSeries[data,index,Name@ps]
	],
	(*else if the index is Null*)
	pandasSeries[Select[Data@ps,func],Null,Name@ps]
	];

(*Type pandasSeries x String -> pandasSeries *)
pandasSeries/:
Part[ps_pandasSeries,str_String]:=
	With[{positions=Flatten@Position[Index@ps,str]},
	pandasSeries[Data[ps][[positions]],
				If[Index@ps=!=Null,ConstantArray[str,Length@positions]],
				Name@ps];
]
 
(*Allow comparison functions to thread over the pandas Series in both directions*)
comparisonFunctions=Less|LessEqual|Equal|GreaterEqual|Greater|Unequal;
pandasSeries/:
func_[ps_pandasSeries,val_]/;MatchQ[func,comparisonFunctions]:=
	pandasSeries[Thread@func[Data@ps,val],Index@ps,Name@ps];

pandasSeries/:
func_[val_,ps_pandasSeries]/;MatchQ[func,comparisonFunctions]:=
	pandasSeries[Thread@func[val,Data@ps],Index@ps,Name@ps];

(*Basic arithmetic*)
arithmeticFuncs=Plus|Minus|Times|Divide;
relationalFuncs= Join|Union|Complement|DeleteDuplicates;

pandasSeries/:
Join[psa_pandasSeries,psb_pandasSeries]:=
	pandasSeries[Join[Data@psa,Data@psb],
				With[
					{indexa=Index@psa/.Null->ConstantArray[Null,Length@Data@psa],
					indexb=Index@psb/.Null->ConstantArray[Null,Length@Data@psb]},
					Join[indexa,indexb]
					],
				Name@psa
	]

(*Unions the pandasSeries based on their Indexes*)

pandasSeries/:
Union[psa_pandasSeries,psb_pandasSeries]:=
	With[{pairs=Join[Transpose@{Data@psa,Index@psa},Transpose@{Data@psb,Index@psb}]},
			(*TODO rewrite using Composition*)
		DeleteDuplicates[pairs,Last@#1==Last@#2&]//Transpose//Join[#,Name@psa]&//Apply[pandasSeries,#]&
	]

(*Complement based on the Index*)
pandasSeries/:
Complement[psa_pandasSeries,psb_pandasSeries]:=
	If[Index@psa===Null,Throw["FirstArgument must have an index"],
	With[{selections=Select[Transpose@{Data@psa,Index@psa},(Not@MemberQ[Index@psb,Last@#])&]},
	Transpose[selections]/.{data_,index_}->pandasSeries[data,index,Name@psa]
	]
	]

pandasSeries/:
func_[pandasSeries[lsta_List,indexa_],pandasSeries[lstb_List,indexb_]]/;MatchQ[func,arithmeticFuncs]:=
	With[{inBnotA=Complement[indexa,indexb]},
		Map[#&,indexa]
	]

(*This is a strange way of trying to do the iterator pattern since there really isnt a good way of doing it here *)

noItFuncs=Set|SetDelayed|Equal|SameQ|If|With|pandasSeries|MatchQ|FullForm|InputForm|comparisonFunctions|TraditionalForm|InputForm|StandardForm|List|Replace|ReplaceAll|Grid|Index|Data|Name|Rule|RuleDelayed|List|relationalFuncs|Transpose;

pandasSeries/:func_[fore___,ps_pandasSeries,after___]/;Not@MatchQ[func,noItFuncs]:=
	With[{res=func[fore,Data@ps,after]},
		If[Head[res]===List,pandasSeries[res,Index@ps,Name@ps],res]
	];
(*TODO handle functions like split,GatherBy,SplitBy,Sort,SortBy,Select,Tally, DeletDuplicates,etc*)
(*TODO make a warning mechanism when the constructor is given bad input*)(*pandasSeries[lst_,index_]:=
Print["Incorrect arguments given to pandasSeries"]/;Length[index]!=Length[lst]&&Length[index]!=0||Not@VectorQ[lst,NumericQ]||Not@VectorQ[index,StringQ]*)

(*Accesors for the content in the pandas expression*)
pandasSeries/:Index[pandasSeries[_,index_,_]]:=index;
pandasSeries/:Data[pandasSeries[lst_,_,_]]:=lst;
pandasSeries/:Name[pandasSeries[_,_,name_]]:=name;


End[]


EndPackage[]
