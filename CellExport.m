BeginPackage["CellExport`"];
CellExport; TagProperties; CellFilter; ExportBaseFilename; \
ExportFormat; ExportOptions; Split;
Begin["Private`"];
HackedExport[fname_String, rest___] := Module[{out},
   If[FileExtension[fname] =!= "png",
    Export[fname, rest],
    Export["HackedExportTemporary.gif", rest];
    out = Export[fname, Import["HackedExportTemporary.gif"]];
    DeleteFile["HackedExportTemporary.gif"];
    out
    ]
   ];
ConditionalExport[fname_String, rest___] := Module[{temp, exists},
  temp = "ConditionalExportTemporary" <> "." <> FileExtension[fname];
  exists = FileExistsQ[fname];
  Export[temp, rest];
  If[exists && FileByteCount[fname] === FileByteCount[temp],
   DeleteFile[temp],
   Print["Exporting " <> fname <> "..."];
   If[exists, DeleteFile[fname]];
   RenameFile[temp, fname]
   ];
  fname
  ]
TagProperties[_] := {};
Options[CellExport] = {
   PageWidth -> 6, CellFilter -> Identity, 
   ExportBaseFilename -> Automatic, ExportFormat -> ".pdf", 
   ExportOptions -> {}, Split -> False
   };
CellExport[tag_String, opts___Rule] := CellExport[
   NotebookGet[EvaluationNotebook[]],
   tag, opts
   ];
CellExport[nb_Notebook, tag_String] := 
  CellExport[nb, tag, TagProperties[tag]];
CellExport[nb_Notebook, tag_String, OptionsPattern[]] := Module[
   {cells, cell, filename, format},
   filename = OptionValue[ExportBaseFilename] /. Automatic -> tag;
   format = OptionValue[ExportFormat];
   cells = OptionValue[CellFilter][Cases[
      nb, 
      c_Cell /; FreeQ[List @@ c, Cell] && ! FreeQ[c, CellTags -> tag],
      Infinity
      ]];
   If[! OptionValue[Split],
    If[Length[cells] >= 1,
     If[Length[cells] == 1,
      cells = 
       Append[First[cells], PageWidth -> 72 OptionValue[PageWidth]],
      cells = 
       Cell[CellGroup[cells], PageWidth -> 72 OptionValue[PageWidth]]
      ];
     ConditionalExport[
      filename <> format, cells,
      ImageResolution -> 300,
      OptionValue[ExportOptions]
      ]
     ],
    k = 0;
    Table[
     ++k;
     ConditionalExport[
      filename <> "-" <> ToString[k] <> format,
      Append[cell, PageWidth -> 72 OptionValue[PageWidth]],
      ImageResolution -> 300,
      OptionValue[ExportOptions]
      ],
     {cell, cells}
     ]
    ]
   ];
End[]; EndPackage[];
