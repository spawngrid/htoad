%% Only include this file in modules that are exporting a library of definitions

-exprecs_prefix([operation, "_"]).
-exprecs_fname([prefix, record]).
-exprecs_vfname([fname, "__", version]).
  
-compile({parse_transform, exprecs}).
