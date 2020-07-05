{-# LANGUAGE OverloadedStrings #-}

module Expect.Hackage1 where

import HackageJson
import Data.HashMap.Strict (fromList)


hackageJson1 :: HackageJson
hackageJson1 =
  fromList
    [("3dmodels",
      fromList
        [("0.3.0",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r0",
                  revsRevs =
                    fromList
                      [("r0",
                        RevisionData
                          {revDataOutPath =
                             "3dmodels-0.3.0-r0-6e458543040f17207eb83c1a48801116d54c0607c40bc3dfb0a252d196b73a3a.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "nE\133C\EOT\SI\ETB ~\184<\SUBH\128\DC1\SYN\213L\ACK\a\196\v\195\223\176\162R\209\150\183::"})]},
             pkgHash =
               "\EM\250u\133T\188oY\195{\253\&9\v\195\214\155 \209z\141\204\167\EMK\EOT%\165\178\194C\247\STX"})]),
     ("cryptohash-sha256",
      fromList
        [("0.11.100.0",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r0",
                  revsRevs =
                    fromList
                      [("r0",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.100.0-r0-ab82a9dcc4c099ebd6dd79dee2bd445b1bfc45e4ae595a6b9025f9cc269f0677.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "\171\130\169\220\196\192\153\235\214\221y\222\226\189D[\ESC\252E\228\174YZk\144%\249\204&\159\ACKw"})]},
             pkgHash =
               "\212\236q\177h\243`\STX&\203S\172CU\158\181y]]y\144J\145\f&\EOTl6\r`\215\128"}),
         ("0.11.7.1",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r0",
                  revsRevs =
                    fromList
                      [("r0",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.7.1-r0-692e110de9e44507e18849a29b3aba4ae24506a06d96e17daa6f88c464e60088.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "i.\DC1\r\233\228E\a\225\136I\162\155:\186J\226E\ACK\160m\150\225}\170o\136\196d\230\NUL\136"})]},
             pkgHash =
               "\172B\176\216c\223\217\RS\ESCw\245\DC3\211q\247>1\203\147\193gq0\255c\163\191 \196\SUB\139\192"}),
         ("0.11.7.2",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r0",
                  revsRevs =
                    fromList
                      [("r0",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.7.2-r0-aa22880e9c0f08d6599b294ca11998f11b76b810017ef89017563e08bf52b3cc.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "\170\"\136\SO\156\SI\b\214Y\155)L\161\EM\152\241\ESCv\184\DLE\SOH~\248\144\ETBV>\b\191R\179\204"})]},
             pkgHash =
               "\SO\138\176\135L\176\146\150'\139\131\DC2\141\FS\225\&95\205\176\139@\181\219\241\230<\165\128w\SYN)\217"}),
         ("0.11.100.1",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r1",
                  revsRevs =
                    fromList
                      [("r0",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.100.1-r0-f25348d77bf493f08cad42d6df91dafa8fda3c67ea99c716423eae2211e7818d.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "\242SH\215{\244\147\240\140\173B\214\223\145\218\250\143\218<g\234\153\199\SYNB>\174\"\DC1\231\129\141"}),
                       ("r1",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.100.1-r1-0fd2d404c8c1cb3b3b3a810a5d5eaf2ade6f1cc7f30b50ae88d7102f5ca78d7b.nix",
                           revDataNum = 1,
                           revDataCabalHash =
                             "\SI\210\212\EOT\200\193\203;;:\129\n]^\175*\222o\FS\199\243\vP\174\136\215\DLE/\\\167\141{"})]},
             pkgHash =
               "W\176#8\233d\134\&93W\136\180\"\221LtEC\203\t\145\&4tr\226\227b\138\&3\194\245\214"}),
         ("0.11.101.0",
          Package
            {pkgRevs =
               Revisions
                 {revsDefault = "r4",
                  revsRevs =
                    fromList
                      [("r2",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.101.0-r2-2790b29264ebf63dac368dd05ec931254e5c70b0ccbc87d0969903533b32b054.nix",
                           revDataNum = 2,
                           revDataCabalHash =
                             "'\144\178\146d\235\246=\172\&6\141\208^\201\&1%N\\p\176\204\188\135\208\150\153\ETXS;2\176T"}),
                       ("r3",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.101.0-r3-e1fa0b073b9f4add2dc03a75c7fa1774ef1792e8697fa1d3207d8a743cf930ab.nix",
                           revDataNum = 3,
                           revDataCabalHash =
                             "\225\250\v\a;\159J\221-\192:u\199\250\ETBt\239\ETB\146\232i\DEL\161\211 }\138t<\249\&0\171"}),
                       ("r0",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.101.0-r0-038fba767413c9a30dc0f964721713ab9b2853366ef47bd425e922ad56933fbf.nix",
                           revDataNum = 0,
                           revDataCabalHash =
                             "\ETX\143\186vt\DC3\201\163\r\192\249dr\ETB\DC3\171\155(S6n\244{\212%\233\"\173V\147?\191"}),
                       ("r1",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.101.0-r1-e3f29d07fbf359ae5db78dd2867c710c4ffcbc1b0c4b4205823ea8c7abcd71a5.nix",
                           revDataNum = 1,
                           revDataCabalHash =
                             "\227\242\157\a\251\243Y\174]\183\141\210\134|q\fO\252\188\ESC\fKB\ENQ\130>\168\199\171\205q\165"}),
                       ("r4",
                        RevisionData
                          {revDataOutPath =
                             "cryptohash-sha256-0.11.101.0-r4-b963d6308db096362d73d9c603b331cf188aa69310195f479dfedf6045d7e602.nix",
                           revDataNum = 4,
                           revDataCabalHash =
                             "\185c\214\&0\141\176\150\&6-s\217\198\ETX\179\&1\207\CAN\138\166\147\DLE\EM_G\157\254\223`E\215\230\STX"})]},
             pkgHash =
               "Rud5\219\234$\142\&4O\188\188\197\223S\a\246\r\250\207\&3}\253\DC1\174\&0\241\199\164\218\ENQ\221"})])]
