        COMMON/MATGA2/ RMAT21S1(14,2),RMAT21E(7,2),RMAT21A(6,2),
     +RMAT21S2(14,2),RMAT21S3(14,2),
     +RMAT22S1(24,2),RMAT22E(7,2),RMAT22A(6,2),
     +RMAT22S2(24,2),RMAT22S3(24,2),
     +RMAT31S(4,2),RMAT32S(10,2),RMAT33S(10,2),
     +RMAT31E(8,2),RMAT32E(11,2),RMAT31A(10,2),
     +RMAT11S(16,2),RMAT11E(18,2),RMAT11A(17,2),
     +RMAT12S(19,2),RMAT12E(21,2),RMAT12A(13,2),
     +RMAT13S(23,2),RMAT13E(31,2),RMAT13A(30,2),
     +RMAT14S(5,2),RMAT14E(7,2),RMAT14A(6,2),
     +RMAT15S(11,2),RMAT15E(13,2),RMAT15A(12,2)

      COMMON/MATGA3/I11S,I11E,I11A,I12S,I12E,I12A,I13S,I13E,I13A,
     +              I14S,I14E,I14A,I15S,I15E,I15A
      COMMON/MATGA1/I21S,I21E,I21A,I22S,I31S,I31E,I31A,I32S,I32E
      COMMON/MATFLE/RMATS(50,2),RMATE(50,2),RMATA(50,2),I4S,I4E,I4A

        DATA RMAT21S1/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +       136,136,123,106,93,86,78,68,60,52,46,39,33,28/
        DATA RMAT21S2/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +              136,130,116,103,90,86,78,68,60,52,46,39,33,28/
        DATA RMAT21S3/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +              136,126,113,100,90,86,78,68,60,52,46,39,33,28/
        DATA RMAT21E/20,100,200,300,400,500,600,
     +              212,205,200,192,183,175,166/
        DATA RMAT21A/100,200,300,400,500,600,
     +              12.5,13.1,13.6,14.0,14.4,14.7/
        DATA RMAT22S1/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +          490,500,510,520,530,540,550,560,570,580,
     +          150,150,150,150,143,136,135,134,132,131,130,128,125,113,
     +          101,90,78,68,60,52,45,38,34,29/
        DATA RMAT22S2/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +          490,500,510,520,530,540,550,560,570,580,
     +          150,150,150,146,136,130,128,127,126,124,123,122,120,113,
     +          101,90,78,68,60,52,45,38,34,29/
        DATA RMAT22S3/
     +       20,200,250,300,350,400,410,420,430,440,450,460,470,480,
     +          490,500,510,520,530,540,550,560,570,580,
     +          150,150,150,140,130,123,122,120,119,118,116,115,114,112,
     +          101,90,78,68,60,52,45,38,34,29/
        DATA RMAT22E/20,100,200,300,400,500,600,
     +              214,209,202,195,187,177,167/
        DATA RMAT22A/100,200,300,400,500,600,
     +              12.0,13.0,13.0,14.0,14.0,14.0/
        DATA RMAT31S/343,371,399,427,103.4,99.3,89.6,74.4/
        DATA RMAT32S/343,371,399,427,454,482,510,538,566,593,
     +               103.4,103.4,103.4,103.4,99.3,90.3,75.8,
     +               45.5,28.2,20.7/
        DATA RMAT33S/343,371,399,427,454,482,510,538,566,593,
     +               103.4,103.4,103.4,103.4,99.3,90.3,75.8,
     +               53.7,39.9,28.9/
        DATA RMAT31E/21,93,149,204,260,316,371,427,
     +               192.3,191.0,188.9,186.1,182.0,177.2,171.0,161.3/
        DATA RMAT32E/21,93,149,204,260,316,371,427,482,538,593,
     +               206.1,203.4,199.9,197.2,193.0,
     +               188.9,183.4,177.2,168.9,158.6,140.6/
        DATA RMAT31A/93,149,204,260,316,371,427,482,538,593,
     +    11.48,11.88,12.28,12.64,13.01,13.39,13.77,14.11,14.35,14.62/
        DATA RMAT11S/20,250,260,280,300,320,340,350,
     +                  360,380,400,410,420,430,440,450,
     +           111,104,101,96,91,89,84,80,78,75,70,68,66,61,55,49/
        DATA RMAT11E/20,100,200,250,260,280,300,320,340,350,
     +                  360,380,400,410,420,430,440,450,
     +              198,191,181,176,175,173,171,168,166,164,
     +              163,160,157,156,155,155,154,153/
        DATA RMAT11A/100,200,250,260,280,300,320,340,350,
     +                  360,380,400,410,420,430,440,450,
     +              11.90,12.60,12.70,12.72,12.76,12.80,12.84,12.88,
     +        12.90,12.92,12.96,13.00,13.10,13.20,13.30,13.40,13.50/
        DATA RMAT12S/20,250,260,280,300,320,340,350,
     +          360,380,400,410,420,430,440,450,460,470,480,131,125,
     +      123,118,113,109,102,100,97,92,87,83,78,72,63,55,47,41,37/
        DATA RMAT12E/20,100,200,250,260,280,300,320,340,350,
     +                  360,380,400,410,420,430,440,450,460,470,480,
     +              198,183,175,171,170,168,166,165,163,162,161,159,
     +              158,155,153,151,148,146,144,141,129/
        DATA RMAT12A/100,200,250,260,280,300,320,340,350,
     +                  360,380,400,480,
     +        11.16,12.12,12.45,12.52,12.65,12.78,12.99,13.20,13.31,
     +        13.41,13.62,13.83,13.91/
        DATA RMAT13S/20,350,
     +                  360,380,400,410,420,430,440,450,460,470,480,
     +                  490,500,510,520,530,540,550,560,570,580,
     +           147,143,141,138,135,133,132,131,130,128,126,125,124,
     +           121,118,110,98,86,77,71,65,57,50/
        DATA RMAT13E/20,100,200,250,260,280,300,320,340,350,360,
     +                  380,400,410,420,430,440,450,460,470,480,
     +                  490,500,510,520,530,540,550,560,570,580,
     +              208,205,201,197,196,194,192,190,188,187,186,
     +              183,181,180,178,177,175,174,172,170,168,166,
     +              165,163,162,160,158,157,153,153,152/
        DATA RMAT13A/100,200,250,260,280,300,320,340,350,360,
     +               380,400,410,420,430,440,450,460,470,480,
     +               490,500,510,520,530,540,550,560,570,580,
     + 13.60,13.70,13.85,13.88,13.94,14.00,14.04,14.08,14.10,14.12,
     + 14.16,14.20,14.23,14.26,14.29,14.32,14.35,14.38,14.41,14.44,
     + 14.47,14.50,14.52,14.54,14.56,14.58,14.60,14.62,14.64,14.68/
        DATA RMAT14S/20,250,260,280,300,
     +              124,113,111,105,101/
        DATA RMAT14E/20,100,200,250,260,280,300,
     +              206,200,192,188,187,186,184/
        DATA RMAT14A/100,200,250,260,280,300,
     +              12.20,13.00,13.23,13.27,13.36,13.45/
        DATA RMAT15S/20,250,260,280,300,320,340,350,360,380,400,
     +              156,149,146,140,135,132,130,129,127,122,117/
        DATA RMAT15E/20,100,200,250,260,280,300,320,340,350,360,380,400,
     +              206,200,189,185,184,183,181,179,177,176,175,173,171/
        DATA RMAT15A/100,200,250,260,280,300,320,340,350,360,380,400,
     +              8.31,10.99,11.60,11.78,12.05,12.31,12.49,
     +             12.68,12.77,12.86,13.04,13.22/
        DATA I21S,I21E,I21A,I22S/14,7,6,24/
        DATA I31S,I31E,I31A,I32S,I32E/4,8,10,10,11/
      DATA I11S,I11E,I11A,I12S,I12E,I12A,I13S,I13E,I13A,I14S,I14E,I14A,
     +     I15S,I15E,I15A/16,18,17,19,21,13,23,31,30,5,7,6,13,13,12/
