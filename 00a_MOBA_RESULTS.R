# ===================== MAIN RESULTS =====================

# **** INDICES ****

moba_parents <- data.frame(
  Index    = c("Sibcorr", "IOLIB", "IORAD", "diff"),
  Outcome  = "education",
  Estimate = c(0.37420625, 0.31885506, 0.39624053, 0.07738548),
  SE       = c(0.007120446, 0.006584239, 0.006756027, 0.003072360),
  Lower    = c(0.36085688, 0.30675600, 0.38353998, 0.07142064),
  Upper    = c(0.38822480, 0.33108153, 0.40834818, 0.08337556),
  pval     = 0,
  N        = 19514
)

moba_children <- data.frame(
  Index    = c("Sibcorr", "IOLIB", "IORAD", "diff"),
  Outcome  = "education",
  Estimate = c(0.39806344, 0.34671311, 0.43196815, 0.08525504),
  SE       = c(0.011816917, 0.011416635, 0.011416520, 0.005635159),
  Lower    = c(0.37307607, 0.32240406, 0.40813204, 0.07401101),
  Upper    = c(0.4204166, 0.3675627, 0.4543084, 0.0963787),
  pval     = 0,
  N        = 6740
)



# **** VARIANCE COMPONENTS ****

moba_parents_full <- data.frame(
  Outcome      = "education",
  Model        = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
  emptyind     = c(0.6258749, NA, NA),
  emptyfam     = c(0.3742548, NA, NA),
  totalvar     = c(1.00013, NA, NA),
  condind      = c(NA, 0.6167079, NA),
  condfam      = c(NA, 0.3060262, NA),
  completeind  = c(NA, NA, 0.6038377),
  completefam  = c(NA, NA, 0.2887662),
  Sibcorr      = c(0.3742062, NA, NA),
  condcorr     = c(0.3059865, NA, NA),
  w            = c(0.01286852, NA, NA),
  v            = c(0.02203429, NA, NA),
  IOLIB        = c(0.3188551, NA, NA),
  IORAD        = c(NA, NA, 0.3962405)
)

moba_children_full <- data.frame(
  Outcome      = "education",
  Model        = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
  emptyind     = c(0.6017946, NA, NA),
  emptyfam     = c(0.3979695, NA, NA),
  totalvar     = c(0.9997641, NA, NA),
  condind      = c(NA, 0.5822325, NA),
  condfam      = c(NA, 0.3322967, NA),
  completeind  = c(NA, NA, 0.5678979),
  completefam  = c(NA, NA, 0.3193646),
  Sibcorr      = c(0.3980634, NA, NA),
  condcorr     = c(0.3323751, NA, NA),
  w            = c(0.01433805, NA, NA),
  v            = c(0.03390471, NA, NA),
  IOLIB        = c(0.3467131, NA, NA),
  IORAD        = c(NA, NA, 0.4319681)
)




# ===================== GENDER RESULTS =====================

# **** INDICES ****

# -- parents
moba_parents_gender <- data.frame(
  Index    = c("Sibcorr", "IOLIB", "IORAD", "diff", "Sibcorr", "IOLIB", "IORAD", "diff"),
  Outcome  = "education",
  Estimate = c(0.43215919, 0.34448999, 0.44051567, 0.09602568, 0.36735349, 0.30454527, 0.38129516, 0.07674989),
  SE       = c(0.015940813, 0.016311797, 0.015553971, 0.007992736, 0.011298107, 0.010929627, 0.011260915, 0.005077115),
  Lower    = c(0.39823437, 0.31139253, 0.40723606, 0.08058838, 0.34603181, 0.28350671, 0.35866481, 0.06693570),
  Upper    = c(0.46253589, 0.37488900, 0.46957393, 0.11274226, 0.38979932, 0.32589668, 0.40257971, 0.08644664),
  pval     = 0,
  N        = c(3246, 3246, 3246, 3246, 7034, 7034, 7034, 7034),
  ability  = "PGI",
  Sample   = c(rep("Brothers", 4), rep("Sisters", 4))
)

# -- children
moba_children_gender <- data.frame(
  Index    = c("Sibcorr", "IOLIB", "IORAD", "diff", "Sibcorr", "IOLIB", "IORAD", "diff"),
  Outcome  = "education",
  Estimate = c(0.40608267, 0.34796685, 0.42542794, 0.07746108, 0.34817288, 0.31336204, 0.38650049, 0.07313845),
  SE       = c(0.02288889, 0.02176536, 0.02229439, 0.01117333, 0.02569736, 0.02519820, 0.02544296, 0.01098280),
  Lower    = c(0.36192249, 0.30751196, 0.38169583, 0.05692246, 0.29468052, 0.26271273, 0.33685140, 0.05272897),
  Upper    = c(0.45214011, 0.39148251, 0.46966265, 0.09844327, 0.39186774, 0.35868446, 0.43222634, 0.09675889),
  pval     = c(0e+00, 0e+00, 0e+00, 4.129586e-12, 0e+00, 0e+00, 0e+00, 2.750200e-11),
  N        = c(rep(1760, 4), rep(1601, 4)),
  ability  = "PGI",
  Sample   = c(rep("Brothers", 4), rep("Sisters", 4))
)



# **** VARIANCE COMPONENTS ****

moba_parents_brothers_full <- data.frame(
  Outcome      = "education",
  Model        = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
  emptyind     = c(0.5674463, NA, NA),
  emptyfam     = c(0.431859, NA, NA),
  totalvar     = c(0.9993053, NA, NA),
  condind      = c(NA, 0.5665797, NA),
  condfam      = c(NA, 0.3367666, NA),
  completeind  = c(NA, NA, 0.5590957),
  completefam  = c(NA, NA, 0.3268422),
  Sibcorr      = c(0.4321592, NA, NA),
  condcorr     = c(0.3370007, NA, NA),
  w            = c(0.007489263, NA, NA),
  v            = c(0.008356481, NA, NA),
  IOLIB        = c(0.34449, NA, NA),
  IORAD        = c(NA, NA, 0.4405157)
)


moba_parents_sisters_full <- data.frame(
  Outcome      = "education",
  Model        = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
  emptyind     = c(0.632916, NA, NA),
  emptyfam     = c(0.36751, NA, NA),
  totalvar     = c(1.000426, NA, NA),
  condind      = c(NA, 0.6227279, NA),
  condfam      = c(NA, 0.3009155, NA),
  completeind  = c(NA, NA, 0.6189684),
  completefam  = c(NA, NA, 0.2847136),
  Sibcorr      = c(0.3673535, NA, NA),
  condcorr     = c(0.3007874, NA, NA),
  w            = c(0.003757919, NA, NA),
  v            = c(0.01394167, NA, NA),
  IOLIB        = c(0.3045453, NA, NA),
  IORAD        = c(NA, NA, 0.3812952)
)



# ===================== PGI-RC RESULTS =====================

moba_pgi_rc <- data.frame(
  Index    = c("Sibcorr", "IOLIB", "IORAD", "Sibcorr", "IOLIB", "IORAD", "Sibcorr", "IOLIB", "IORAD"),
  Outcome  = "education",
  Estimate = c(0.37, 0.32, 0.40, 0.37, 0.18, 0.42, 0.37, 0.23, 0.41),
  SE       = 0.01,
  Lower    = c(0.36, 0.30, 0.39, 0.36, 0.16, 0.40, 0.36, 0.21, 0.40),
  Upper    = c(0.39, 0.33, 0.41, 0.39, 0.19, 0.44, 0.39, 0.24, 0.43),
  pval     = 0,
  N        = 19514,
  Panel    = c(rep("Original", 3), rep("Corrected (Becker et al.)", 3), rep("Corrected (Howe et al.)", 3)),
  rhoB     = c(NA, NA, NA, 1.76, 1.76, 1.76, 1.53, 1.53, 1.53),
  rhoW     = c(NA, NA, NA, 1.98, 1.98, 1.98, 1.72, 1.72, 1.72)
) %>% mutate(
  Index = factor(Index, levels = INDICES),
  Panel = factor(Panel, levels = c("Original", "Corrected (Becker et al.)","Corrected (Howe et al.)"))
)




# ===================== SINGLE PGIs RESULTS =====================

moba_single_pgis <- data.frame(
  Index    = rep(c("Sibcorr", "IOLIB", "IORAD", "diff"), 6),
  Outcome  = "education",
  Estimate = c(
    3.742062e-01, 3.190472e-01, 3.961394e-01,  7.709212e-02,
    3.742062e-01, 3.691530e-01, 3.884830e-01,  1.933001e-02,
    3.742062e-01, 3.858366e-01, 3.857919e-01, -4.473387e-05,
    3.742062e-01, 3.852069e-01, 3.860218e-01,  8.148588e-04,
    3.742062e-01, 3.857034e-01, 3.860703e-01,  3.669111e-04,
    3.742062e-01, 3.858458e-01, 3.857913e-01, -5.454947e-05
  ),
  SE = c(
    7.279268e-03, 7.302255e-03, 6.973121e-03, 2.747617e-03,
    6.659981e-03, 6.510157e-03, 6.328945e-03, 1.787825e-03,
    6.277167e-03, 5.947362e-03, 5.949483e-03, 5.600876e-05,
    6.818628e-03, 6.471029e-03, 6.519448e-03, 4.201260e-04,
    6.698587e-03, 6.536711e-03, 6.575608e-03, 2.367980e-04,
    7.069640e-03, 6.833217e-03, 6.839313e-03, 6.217241e-05
  ),
  Lower = c(
    3.587253e-01, 3.038079e-01, 3.811037e-01,  7.172719e-02,
    3.607575e-01, 3.560795e-01, 3.750676e-01,  1.586709e-02,
    3.642296e-01, 3.755370e-01, 3.754457e-01, -9.317557e-05,
    3.624800e-01, 3.725385e-01, 3.733318e-01,  2.847131e-04,
    3.620247e-01, 3.744685e-01, 3.747232e-01, -1.372646e-05,
    3.603135e-01, 3.727426e-01, 3.727064e-01, -9.623642e-05
  ),
  Upper = c(
    0.3884914658, 0.3304717721, 0.4090619917, 0.0818310519,
    0.3858908746, 0.3800965472, 0.3998373267, 0.0223345384,
    0.3878881638, 0.3990886672, 0.3990011537, 0.0001071262,
    0.3856913318, 0.3966856542, 0.3974035681, 0.0018320654,
    0.3886516066, 0.3991931039, 0.3997680099, 0.0009012905,
    0.3860005826, 0.3984839802, 0.3984202421, 0.0001769038
  ),
  pval = c(
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0.42446777,
    0, 0, 0, 0.05243345,
    0, 0, 0, 0.12126906,
    0, 0, 0, 0.38027474
  ),
  N   = 19514,
  PGI = rep(c("pgi_education", "pgi_cognitive", "pgi_extraversion", 
              "pgi_neuroticism", "pgi_openness", "pgi_risk"), each = 4)
)

