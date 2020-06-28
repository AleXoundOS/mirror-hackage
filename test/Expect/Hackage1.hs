{-# LANGUAGE OverloadedStrings #-}

module Expect.Hackage1 where

import HackageJson
import Data.HashMap.Strict (fromList)


hackageJson1 :: Either String HackageJson
hackageJson1 = Right
    ( fromList
        [
            ( "3dmodels"
            , fromList
                [
                    ( "0.3.0"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r0"
                            , revsRevs = fromList
                                [
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "3dmodels-0.3.0-r0-6e458543040f17207eb83c1a48801116d54c0607c40bc3dfb0a252d196b73a3a.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "6e458543040f17207eb83c1a48801116d54c0607c40bc3dfb0a252d196b73a3a"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "19fa758554bc6f59c37bfd390bc3d69b20d17a8dcca7194b0425a5b2c243f702"
                        }
                    )
                ]
            )
        ,
            ( "cryptohash-sha256"
            , fromList
                [
                    ( "0.11.100.0"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r0"
                            , revsRevs = fromList
                                [
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.100.0-r0-ab82a9dcc4c099ebd6dd79dee2bd445b1bfc45e4ae595a6b9025f9cc269f0677.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "ab82a9dcc4c099ebd6dd79dee2bd445b1bfc45e4ae595a6b9025f9cc269f0677"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "d4ec71b168f3600226cb53ac43559eb5795d5d79904a910c26046c360d60d780"
                        }
                    )
                ,
                    ( "0.11.7.1"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r0"
                            , revsRevs = fromList
                                [
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.7.1-r0-692e110de9e44507e18849a29b3aba4ae24506a06d96e17daa6f88c464e60088.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "692e110de9e44507e18849a29b3aba4ae24506a06d96e17daa6f88c464e60088"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "ac42b0d863dfd91e1b77f513d371f73e31cb93c1677130ff63a3bf20c41a8bc0"
                        }
                    )
                ,
                    ( "0.11.7.2"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r0"
                            , revsRevs = fromList
                                [
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.7.2-r0-aa22880e9c0f08d6599b294ca11998f11b76b810017ef89017563e08bf52b3cc.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "aa22880e9c0f08d6599b294ca11998f11b76b810017ef89017563e08bf52b3cc"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "0e8ab0874cb09296278b83128d1ce13935cdb08b40b5dbf1e63ca580771629d9"
                        }
                    )
                ,
                    ( "0.11.100.1"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r1"
                            , revsRevs = fromList
                                [
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.100.1-r0-f25348d77bf493f08cad42d6df91dafa8fda3c67ea99c716423eae2211e7818d.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "f25348d77bf493f08cad42d6df91dafa8fda3c67ea99c716423eae2211e7818d"
                                        }
                                    )
                                ,
                                    ( "r1"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.100.1-r1-0fd2d404c8c1cb3b3b3a810a5d5eaf2ade6f1cc7f30b50ae88d7102f5ca78d7b.nix"
                                        , revDataNum = 1
                                        , revDataCabalHash = "0fd2d404c8c1cb3b3b3a810a5d5eaf2ade6f1cc7f30b50ae88d7102f5ca78d7b"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "57b02338e9648639335788b422dd4c744543cb0991347472e2e3628a33c2f5d6"
                        }
                    )
                ,
                    ( "0.11.101.0"
                    , Package
                        { pkgRevs = Revisions
                            { revsDefault = "r4"
                            , revsRevs = fromList
                                [
                                    ( "r2"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.101.0-r2-2790b29264ebf63dac368dd05ec931254e5c70b0ccbc87d0969903533b32b054.nix"
                                        , revDataNum = 2
                                        , revDataCabalHash = "2790b29264ebf63dac368dd05ec931254e5c70b0ccbc87d0969903533b32b054"
                                        }
                                    )
                                ,
                                    ( "r3"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.101.0-r3-e1fa0b073b9f4add2dc03a75c7fa1774ef1792e8697fa1d3207d8a743cf930ab.nix"
                                        , revDataNum = 3
                                        , revDataCabalHash = "e1fa0b073b9f4add2dc03a75c7fa1774ef1792e8697fa1d3207d8a743cf930ab"
                                        }
                                    )
                                ,
                                    ( "r0"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.101.0-r0-038fba767413c9a30dc0f964721713ab9b2853366ef47bd425e922ad56933fbf.nix"
                                        , revDataNum = 0
                                        , revDataCabalHash = "038fba767413c9a30dc0f964721713ab9b2853366ef47bd425e922ad56933fbf"
                                        }
                                    )
                                ,
                                    ( "r1"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.101.0-r1-e3f29d07fbf359ae5db78dd2867c710c4ffcbc1b0c4b4205823ea8c7abcd71a5.nix"
                                        , revDataNum = 1
                                        , revDataCabalHash = "e3f29d07fbf359ae5db78dd2867c710c4ffcbc1b0c4b4205823ea8c7abcd71a5"
                                        }
                                    )
                                ,
                                    ( "r4"
                                    , RevisionData
                                        { revDataOutPath = "cryptohash-sha256-0.11.101.0-r4-b963d6308db096362d73d9c603b331cf188aa69310195f479dfedf6045d7e602.nix"
                                        , revDataNum = 4
                                        , revDataCabalHash = "b963d6308db096362d73d9c603b331cf188aa69310195f479dfedf6045d7e602"
                                        }
                                    )
                                ]
                            }
                        , pkgHash = "52756435dbea248e344fbcbcc5df5307f60dfacf337dfd11ae30f1c7a4da05dd"
                        }
                    )
                ]
            )
        ]
    )
