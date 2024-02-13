WITH
  Table_refor AS (
  SELECT
    DATE(_PARTITIONTIME) AS date,
    R.horodate,
    grpSource,
    idvisite,
    idrecherche,
    CAST(isclicked AS INT64) AS isclicked,
    COALESCE(R.requete.req_quiquoiogr.terme,r.quoiqui) AS quoiqui,
    ouvaleur,
    typerechercheagg
  FROM
    `paj-mediaproc-prd-prj-dataproc.dw_ssa_audience.MODELEH_USER_WB` AS root,
    root.visites AS V,
    V.recherches AS R,
    R.history H,
    H.bandeaux B,
    B.lrs lr
  WHERE
    DATE(_PARTITIONTIME) BETWEEN DATE_SUB(CURRENT_DATE(), INTERVAL 5 DAY )
    AND CURRENT_DATE()
    AND h.isvalidatedbyatinternet=TRUE
    AND robotdetectionlevel=0 /*trafic hors robot*/
    AND rep_univers = 'YES' /*univers Pages Jaunes*/
    AND typesource !='Api' /*hors trafic partenaire*/
    AND isanonymous=FALSE /*filtrage pour la RGPD : requete non anonymisées*/
    AND typereponse = "AVEC_REPONSE"
    AND v.at.pays="France"
    AND interne.domaine IS NULL
    AND ((isValidatedBySearch=TRUE)
      OR (typeaccesrecherche='LR SEO'))
  GROUP BY
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9),
  Table_agg_refo AS (
  SELECT
    *,
    FIRST_VALUE(typerechercheagg)OVER (PARTITION BY idvisite ORDER BY horodate ) AS First_typerechercheagg,
    LEAD(horodate) OVER (PARTITION BY idvisite ORDER BY horodate ASC) AS horodate_suivant,
    LEAD(quoiqui) OVER (PARTITION BY idvisite ORDER BY horodate ASC) AS quoiqui_suivant,
    LEAD(ouvaleur) OVER (PARTITION BY idvisite ORDER BY horodate ASC) AS ouvaleur_suivant,
    LEAD(isclicked) OVER (PARTITION BY idvisite ORDER BY horodate ASC) AS iscliked_suivant
  FROM
    Table_refor --
  WHERE
    -- idvisite = "483323-1000026"
  ORDER BY
    idvisite),
  Table_fin_refo AS (
  SELECT
    *,
    DATETIME_DIFF(horodate_suivant, horodate,SECOND) AS seconde
  FROM
    Table_agg_refo),
  Table_fin AS (
  SELECT
    idvisite,
    idrecherche,
    CASE
      WHEN (seconde<=30) AND (quoiqui!=quoiqui_suivant) AND (ouvaleur = ouvaleur_suivant) THEN idrecherche
  END
    AS flag_rech_avec_refo_quoiqui,
    CASE
      WHEN (seconde<=30) AND (quoiqui=quoiqui_suivant) AND (ouvaleur!=ouvaleur_suivant) THEN idrecherche
  END
    AS flag_rech_avec_refo_ouvaleur,
  FROM
    Table_fin_refo)
SELECT
  COUNT(DISTINCT idrecherche) AS nb_rech,
  COUNT(DISTINCT flag_rech_avec_refo_quoiqui) AS nb_rech_refo_quoiqui,
  COUNT(DISTINCT flag_rech_avec_refo_ouvaleur) AS nb_rech_refo_ouvaleur
FROM
  Table_fin
