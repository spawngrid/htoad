-module(htoad_stdlib).

-include_lib("htoad/include/library.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export_records([init,'htoad.toadie',htoad,
                 user, group, file, package, shell,
                 error_report]).


