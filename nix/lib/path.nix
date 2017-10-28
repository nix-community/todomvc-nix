{ lib }:
# The missing stdlib path functions
let
  inherit (builtins) substring stringLength match;
in
{
  # Works like builtins.filterSource except that:
  # * the path is relative to the input path
  # * the filter has to return false to remove an items
  # This is more natural to most of the use-cases.
  #
  # Filter is used to avoid adding extra files to the /nix/store.
  filterOut = filter: path:
    let
      pathStr = toString path;
      pathSubStart = stringLength pathStr + 1;
      relativeFilter = name: type:
        # cheat and select a reeeealy long end of string
        let path' = substring pathSubStart 65536 name; in
        !(filter path' type);
    in
      builtins.filterSource relativeFilter path;

  # Make it such that compose filterA filterB
  composeFilters = a: b: name: type: (a name type) || (b name type);

  # Create a filter that removes all the mentioned relative paths
  filterElems = elems:
    (name: type: builtins.elem name elems);

  # Inverted lib.cleanSource to work with this filter
  cleanSourceFilter = (name: type: !(lib.cleanSourceFilter name type));
}
