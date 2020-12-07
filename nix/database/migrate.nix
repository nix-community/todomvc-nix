{ symlinkJoin, sqitchPg, makeWrapper, postgresql }:
symlinkJoin {
  name = "migrate";
  buildInputs = [ makeWrapper ];
  paths = [ sqitchPg ./sql ];
  postBuild = ''
    wrapProgram "$out/bin/sqitch" \
      --add-flags "--chdir $out" \
      --set SQITCH_CONFIG $out/sqitch.conf
  '';
}
