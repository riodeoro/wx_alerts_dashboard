# Copyright 2025 Province of British Columbia
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# global_resources.R

tables <- c(
    "dbo.AFTON", "dbo.AKOKLI_CREEK", "dbo.ALEXIS_CREEK", "dbo.ALLISON_PASS", 
    "dbo.ANAHIM_LAKE", "dbo.ANDERSON_CREEK", "dbo.APE_LAKE", "dbo.ASHNOLA", 
    "dbo.ASPEN_GROVE", "dbo.ATLIN", "dbo.AUGIER_LAKE", "dbo.AUGUST_LAKE", 
    "dbo.BALDFACE", "dbo.BEAR_LAKE", "dbo.BEAVER_CREEK", "dbo.BEAVERDELL", 
    "dbo.BEDNESTI", "dbo.BELLIRVING", "dbo.BENSON", "dbo.BERRY", 
    "dbo.BIG_MOUTH_2", "dbo.BIG_SILVER_2", "dbo.BIG_VALLEY", "dbo.BIGATTINI", 
    "dbo.BLACKPINE", "dbo.BLAEBERRY", "dbo.BLOWDOWN_PASS_PROJECT", "dbo.BLUE_RIVER_2", 
    "dbo.BLUEBERRY", "dbo.BOB_QUINN_LAKE", "dbo.BOOTHROYD", "dbo.BOWRON_HAGGEN", 
    "dbo.BOWSER", "dbo.BOYA_LAKE", "dbo.BRENDA_MINES", "dbo.BRISCO", 
    "dbo.BURNS_LAKE_850M", "dbo.CAHILTY", "dbo.CARIBOO_CREEK", "dbo.CATFISH", 
    "dbo.CEDAR", "dbo.CEDARVALE", "dbo.CHEAKAMUS", "dbo.CHEF_CREEK_TEST", 
    "dbo.CHERRY_LAKE", "dbo.CHETWYND_FB", "dbo.CHILAKO", "dbo.CHURN_CREEK", 
    "dbo.CLEARWATER_HUB", "dbo.COLDSCAUR_LAKE", "dbo.CRANBERRY", "dbo.CRANBROOK", 
    "dbo.CRAWFORD", "dbo.CURWEN_CREEK", "dbo.DARCY", "dbo.DARKWOODS", 
    "dbo.DEASE_LAKE_FS", "dbo.DECEPTION", "dbo.DEWAR_CREEK", "dbo.DOWNIE", 
    "dbo.DUNCAN", "dbo.EAST_BARRIERE", "dbo.EAST_OOTSA", "dbo.EIGHT_MILE", 
    "dbo.ELK_MOUNTAIN", "dbo.ELKO", "dbo.EMILY_CREEK", "dbo.FALLS_CREEK", 
    "dbo.FINTRY", "dbo.FIRESIDE", "dbo.FIVE_MILE", "dbo.FLATHEAD_2", 
    "dbo.FORD_MOUNTAIN", "dbo.FORT_NELSON_FS", "dbo.FORT_ST_JAMES", "dbo.FRANK_CREEK", 
    "dbo.FRENCH_BAR", "dbo.FROTHWIND", "dbo.GANOKWA", "dbo.GASPARD", 
    "dbo.GAVIN", "dbo.GLIMPSE", "dbo.GOATFELL", "dbo.GOATHAVEN", 
    "dbo.GOATLICK", "dbo.GOLD_HILL", "dbo.GOLDSTREAM_2", "dbo.GOSNEL", 
    "dbo.GRAHAM", "dbo.GRAND_FORKS", "dbo.GRASSY_PLAINS_HUB", "dbo.GREENSTONE_HUB", 
    "dbo.GWYNETH_LAKE", "dbo.HAGENSBORG_2", "dbo.HAIG_CAMP", "dbo.HARBOUR_LAKE", 
    "dbo.HELMUT", "dbo.HIXON", "dbo.HOLY_CROSS_2", "dbo.HOMATHKO", 
    "dbo.HONNA", "dbo.HORSEFLY", "dbo.HOUSTON", "dbo.HUDSON_HOPE", 
    "dbo.IDABEL_LAKE_3", "dbo.INGENIKA_POINT", "dbo.ISKUT", "dbo.JERRY", 
    "dbo.JOHNSON_LAKE", "dbo.KETCHIKA", "dbo.KETTLE_2", "dbo.KISPIOX_HUB", 
    "dbo.KITPARK", "dbo.KLINAKLINI", "dbo.KLUSKUS", "dbo.KNIFE", 
    "dbo.KOMIE", "dbo.KOOCANUSA", "dbo.LARCH_HILLS_WEST", "dbo.LAVINA_SF", 
    "dbo.LEIGHTON_LAKE", "dbo.LEMORAY", "dbo.LEO_CREEK", "dbo.LIKELY_RS", 
    "dbo.LILLOOET", "dbo.LITTLE_CHOPAKA", "dbo.LONE_BUTTE", "dbo.LOVELL_COVE", 
    "dbo.MABEL_LAKE_2", "dbo.MACHMELL", "dbo.MACHMELL_KLINIKLINI", "dbo.MACKENZIE_FS", 
    "dbo.MANSON", "dbo.MARION", "dbo.MATHEW", "dbo.MAYSON", 
    "dbo.MCBRIDE", "dbo.MCBRIDE_LAKE", "dbo.MCCUDDY", "dbo.McGREGOR_2", 
    "dbo.MCLEAN_LAKE", "dbo.MCLEOD_LAKE", "dbo.MEADOW_LAKE", "dbo.MEAGER_CREEK", 
    "dbo.MENZIES_CAMP", "dbo.MERRITT_2_HUB", "dbo.MESACHIE_2", "dbo.MIDDLE_LAKE", 
    "dbo.MOOSE_LAKE", "dbo.MOUNT_CAYLEY", "dbo.MUDPIT", "dbo.MUSKWA", 
    "dbo.NABESHE", "dbo.NADINA", "dbo.NAHATLATCH", "dbo.NANCY_GREENE", 
    "dbo.NASS_CAMP", "dbo.NAZKO", "dbo.NELSON_FORKS", "dbo.NEMIAH", 
    "dbo.NICOLL", "dbo.NILKITKWA", "dbo.NOEL", "dbo.NORNS", 
    "dbo.NORTH_BABINE", "dbo.NORTH_CHILCO", "dbo.OCTOPUS_CREEK", "dbo.OLD_FADDY", 
    "dbo.OSBORN", "dbo.PADDY", "dbo.PALLISER", "dbo.PARROTT", 
    "dbo.PASKA_LAKE", "dbo.PEDEN", "dbo.PEMBERTON_BASE", "dbo.PENDOREILLE", 
    "dbo.PENTICTON_RS", "dbo.PINE_CREEK", "dbo.PINK_MOUNTAIN", "dbo.PLACE_LAKE", 
    "dbo.PLUMMER_HUT", "dbo.POWDER_CREEK", "dbo.POWELL_RIVER_WEST_LAKE", "dbo.PRAIRIE_CREEK", 
    "dbo.QUINSAM_BASE_FWX", "dbo.RED_DEER", "dbo.REVELSTOKE", "dbo.RISKE_CREEK", "dbo.ROCK_CREEK",  
    "dbo.ROCKINGHAM", "dbo.RORY_CREEK", "dbo.ROSSWOOD", "dbo.ROUND_PRAIRIE", 
    "dbo.SALTSPRING_2", "dbo.SAWTOOTH", "dbo.SCAR_CREEK", "dbo.SEVEREID", 
    "dbo.SEYMOUR_ARM", "dbo.SICAMOUS", "dbo.SIERRA", "dbo.SIFTON", 
    "dbo.SILVER", "dbo.SILVERSTAR_SF", "dbo.SKOONKA", "dbo.SLOCAN", 
    "dbo.SMALLWOOD", "dbo.SPARKS_LAKE", "dbo.SPARWOOD", "dbo.SPLINTLUM", 
    "dbo.STATION_BAY_2", "dbo.SUCCOUR_CREEK", "dbo.SUMMIT", "dbo.SUSTUT", 
    "dbo.TABERNACLE_WIND", "dbo.TABLE_RIVER", "dbo.TALCHAKO", "dbo.TATLA_LAKE", 
    "dbo.TAUTRI", "dbo.TELEGRAPH_CREEK", "dbo.TERRACE", "dbo.THYNNE", 
    "dbo.TIMOTHY", "dbo.TOAD_RIVER", "dbo.TOBA_CAMP", "dbo.TOBY_HUB", 
    "dbo.TROUT_LAKE", "dbo.TS_ARTLISH", "dbo.TS_ATLUCK", "dbo.TS_BLACKWATER", 
    "dbo.TS_BURMAN", "dbo.TS_EFFINGHAM", "dbo.TS_ELPHINSTONE", "dbo.TS_MAURELLE", 
    "dbo.TS_MCNABB", "dbo.TS_NAHMINT", "dbo.TS_NAKA_CREEK", "dbo.TS_SAN_JUAN", 
    "dbo.TS_THEODOSIA", "dbo.TSAR_CREEK", "dbo.TUMBLER_HUB", "dbo.TURTLE", 
    "dbo.UBC_RESEARCH", "dbo.UPPER_FULTON", "dbo.UPPER_KISPIOX", "dbo.VALEMOUNT_2", 
    "dbo.VALEMOUNT_AIRPORT", "dbo.VAN_DYKE", "dbo.VANDERHOOF_HUB", "dbo.WELLS_GRAY", 
    "dbo.WEST_KELOWNA", "dbo.WHITE_RIVER", "dbo.WINDY_MOUNTAIN", "dbo.WITCH", 
    "dbo.WONOWON", "dbo.WOSS_CAMP", "dbo.YOUNG_LAKE", "dbo.ForrestKerr"
)

tables <- sapply(tables, function(x) {
  parts <- strsplit(x, "\\.")[[1]]
  parts[length(parts)]
})
fire_zone_mapping <- list(
  # Northwest Fire Centre Zones
  "Cassiar Fire Zone" = c("ATLIN", "BOB QUINN LAKE", "BOYA LAKE", "DEASE LAKE FS", 
                         "FORRESTKERR", "ISKUT", "OLD FADDY", "TELEGRAPH CREEK"),
  "Nadina Fire Zone" = c("AUGIER LAKE", "BURNS LAKE 850M", "EAST OOTSA", 
                        "GRASSY PLAINS HUB", "HOUSTON", "MCBRIDE LAKE", "NADINA", 
                        "NORTH BABINE", "PARROTT", "PEDEN"),
  "Bulkley Fire Zone" = c("CEDARVALE", "CRANBERRY", "GANOKWA", "KISPIOX HUB", 
                         "NILKITKWA", "PINE CREEK", "UPPER FULTON", "UPPER KISPIOX"),
  "Skeena Fire Zone" = c("BELLIRVING", "KITPARK", "NASS CAMP", "ROSSWOOD", 
                         "TERRACE", "VAN DYKE"),

  # Prince George Fire Centre Zones
  "Prince George Zone" = c("BEAR LAKE", "BEDNESTI", "BOWRON HAGGEN", "CATFISH",
                          "CHILAKO", "HIXON", "JERRY", "McGREGOR 2", "MCLEOD LAKE",
                          "SEVEREID", "TABLE RIVER"),
  "VanJam Zone" = c("FORT ST JAMES", "HOLY CROSS 2", "KLUSKUS", "LEO CREEK",
                    "LOVELL COVE", "MOOSE LAKE", "NORTH CHILCO", "SAWTOOTH",
                    "SUSTUT", "VANDERHOOF HUB", "WITCH"),
  "Mackenzie Zone" = c("BLACKPINE", "INGENIKA POINT", "MACKENZIE FS", "MANSON",
                       "NABESHE", "SIFTON"),
  "Robson Valley Zone" = c("BLUE RIVER 2", "BLUEBERRY", "GOATLICK", "GOSNEL",
                          "MCBRIDE", "ROCKINGHAM", "VALEMOUNT 2", "VALEMOUNT AIRPORT"),
  "Fort Nelson Zone" = c("ELK MOUNTAIN", "FIRESIDE", "FORT NELSON FS", "HELMUT",
                        "KETCHIKA", "KOMIE", "MUSKWA", "NELSON FORKS", "SIERRA",
                        "TOAD RIVER"),
  "Fort St. John Zone" = c("GRAHAM", "OSBORN", "PADDY", "PINK MOUNTAIN", "SILVER",
                          "WONOWON"),
  "Dawson Creek Zone" = c("CHETWYND FB", "HUDSON HOPE", "LEMORAY", "NOEL",
                         "RED DEER", "TUMBLER HUB"),

  # Cariboo Fire Centre Zones
  "Chilcotin Fire Zone" = c("ALEXIS CREEK", "ANAHIM LAKE", "BALDFACE",
                           "MIDDLE LAKE", "NEMIAH", "PLUMMER HUT", "TATLA LAKE"),
  "Central Cariboo Fire Zone" = c("CHURN CREEK", "GASPARD", "GAVIN", "HORSEFLY",
                                 "KNIFE", "LIKELY RS", "PLACE LAKE", "PRAIRIE CREEK",
                                 "RISKE CREEK", "TAUTRI"),
  "Quesnel Fire Zone" = c("BENSON", "BIG VALLEY", "MATHEW", "NAZKO"),
  "100 Mile House Fire Zone" = c("DECEPTION", "LONE BUTTE", "MEADOW LAKE",
                                "TIMOTHY", "WINDY MOUNTAIN", "YOUNG LAKE"),

  # Southeast Fire Centre Zones
  "Arrow Zone" = c("FALLS CREEK", "NANCY GREENE", "OCTOPUS CREEK", "SLOCAN",
                   "TROUT LAKE"),
  "Boundary Zone" = c("AKOKLI CREEK", "BIG MOUTH 2", "GRAND FORKS", "ROCK CREEK", "NICOLL"),
  "Columbia Fire Zone" = c("BIGATTINI", "BRISCO", "CHERRY LAKE", "DARKWOODS", 
                          "DUNCAN", "GOLDSTREAM 2", "MARION", "REVELSTOKE",
                          "SUCCOUR CREEK", "TABERNACLE WIND", "TSAR CREEK"),
  "Cranbrook Fire Zone" = c("BLAEBERRY", "CRANBROOK", "CRAWFORD", "DOWNIE",
                           "ELKO", "FLATHEAD 2", "GOATHAVEN", "KOOCANUSA",
                           "NORNS", "ROUND PRAIRIE", "SPARWOOD"),
  "Invermere Fire Zone" = c("CARIBOO CREEK", "EMILY CREEK", "JOHNSON LAKE",
                           "PALLISER", "TOBY HUB", "WHITE RIVER"),
  "Kootenay Fire Zone" = c("BEAVERDELL", "DEWAR CREEK", "EIGHT MILE",
                          "GOATFELL", "GOLD HILL", "LAVINA SF", "PENDOREILLE",
                          "POWDER CREEK", "RORY CREEK", "SMALLWOOD"),

  # Kamloops Fire Centre Zones
  "Kamloops Zone" = c("AFTON", "BERRY", "CAHILTY", "CLEARWATER HUB",
                      "COLDSCAUR LAKE", "EAST BARRIERE", "FROTHWIND",
                      "GREENSTONE HUB", "HARBOUR LAKE", "LEIGHTON LAKE",
                      "MAYSON", "MCLEAN LAKE", "MUDPIT", "PASKA LAKE",
                      "SPARKS LAKE"),
  "Lillooet Zone" = c("BLOWDOWN PASS PROJECT", "BRENDA MINES", "FIVE MILE",
                      "FRENCH BAR", "GWYNETH LAKE", "LILLOOET", "SKOONKA",
                      "SPLINTLUM"),
  "Merritt Zone" = c("ASPEN GROVE", "AUGUST LAKE", "GLIMPSE", "MERRITT 2 HUB",
                     "THYNNE"),
  "Penticton Zone" = c("ASHNOLA", "IDABEL LAKE 3", "LITTLE CHOPAKA",
                       "MCCUDDY", "PENTICTON RS", "WELLS GRAY"),
  "Vernon Zone" = c("CURWEN CREEK", "FINTRY", "KETTLE 2", "LARCH HILLS WEST",
                    "MABEL LAKE 2", "SEYMOUR ARM", "SICAMOUS", "SILVERSTAR SF",
                    "STATION BAY 2", "TURTLE", "WEST KELOWNA"),

  # Coastal Fire Centre Zones
  "Fraser Zone" = c("ALLISON PASS", "ANDERSON CREEK", "BIG SILVER 2",
                    "BOOTHROYD", "FORD MOUNTAIN", "FRANK CREEK", "HAIG CAMP",
                    "HONNA", "NAHATLATCH", "UBC RESEARCH"),
  "Mid Island Zone" = c("BEAVER CREEK", "BOWSER", "CEDAR", "CHEF CREEK TEST",
                        "SUMMIT", "TS EFFINGHAM", "TS NAHMINT"),
  "North Island Mid Coast Zone (Campbell River)" = c("MENZIES CAMP",
                                                    "QUINSAM BASE FWX",
                                                    "TS ARTLISH", "TS ATLUCK",
                                                    "TS BLACKWATER", "TS BURMAN",
                                                    "TS NAKA CREEK"),
  "North Island Mid Coast Zone (Mid Coast)" = c("APE LAKE", "HAGENSBORG 2",
                                               "MACHMELL", "MACHMELL KLINIKLINI",
                                               "TALCHAKO"),
  "North Island Mid Coast Zone (Port McNeill)" = c("KLINAKLINI", "WOSS CAMP"),
  "Pemberton Zone" = c("CHEAKAMUS", "DARCY", "MEAGER CREEK", "MOUNT CAYLEY",
                       "PEMBERTON BASE"),
  "South Island Zone" = c("MESACHIE 2", "SALTSPRING 2", "TS SAN JUAN"),
  "Sunshine Coast Zone" = c("HOMATHKO", "POWELL RIVER WEST LAKE", "SCAR CREEK",
                           "TOBA CAMP", "TS ELPHINSTONE", "TS MAURELLE",
                           "TS MCNABB", "TS THEODOSIA")
)

# Create fire center mapping
fire_center_mapping <- list(
  "Northwest Fire Centre" = c("Cassiar Fire Zone", "Nadina Fire Zone", 
                            "Bulkley Fire Zone", "Skeena Fire Zone"),
  
  "Prince George Fire Centre" = c("Prince George Zone", "VanJam Zone", 
                                "Mackenzie Zone", "Robson Valley Zone",
                                "Fort Nelson Zone", "Fort St. John Zone",
                                "Dawson Creek Zone"),
  
  "Cariboo Fire Centre" = c("Chilcotin Fire Zone", "Central Cariboo Fire Zone",
                           "Quesnel Fire Zone", "100 Mile House Fire Zone"),
  
  "Southeast Fire Centre" = c("Arrow Zone", "Boundary Zone", "Columbia Fire Zone",
                            "Cranbrook Fire Zone", "Invermere Fire Zone",
                            "Kootenay Fire Zone"),
  
  "Kamloops Fire Centre" = c("Kamloops Zone", "Lillooet Zone", "Merritt Zone",
                            "Penticton Zone", "Vernon Zone"),
  
  "Coastal Fire Centre" = c("Fraser Zone", "Mid Island Zone",
                           "North Island Mid Coast Zone (Campbell River)",
                           "North Island Mid Coast Zone (Mid Coast)",
                           "North Island Mid Coast Zone (Port McNeill)",
                           "Pemberton Zone", "South Island Zone",
                           "Sunshine Coast Zone")
)
