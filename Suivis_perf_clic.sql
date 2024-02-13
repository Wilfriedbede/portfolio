WITH Table_test AS(SELECT  

DISTINCT 
  idrecherche , 
  CASE WHEN P_LR.horodate IS NOT NULL THEN TRUE ELSE FALSE END AS is_lr, 
  CASE WHEN P_FD.horodate IS NOT NULL THEN TRUE ELSE FALSE END AS is_fd, 
  CASE WHEN SAFE_CAST(c_FD.co_type AS STRING) IN ("0","1") THEN TRUE ELSE FALSE END AS is_clic_fd, 
  CASE WHEN SAFE_CAST(c_LR.co_type AS STRING) IN ("0","1") THEN TRUE ELSE FALSE END AS is_clic_lr, 
  CASE WHEN SAFE_CAST(c_FD.co_type AS STRING) IN ("2","3") THEN TRUE ELSE FALSE END AS is_contact_fd, 
  CASE WHEN SAFE_CAST(c_LR.co_type AS STRING) IN ("2","3") THEN TRUE ELSE FALSE END AS is_contact_lr, 
  CASE WHEN SAFE_CAST(c_LR.co_type AS STRING) IN ("2","3") OR SAFE_CAST(c_FD.co_type AS STRING) IN ("2","3")  THEN TRUE ELSE FALSE END AS is_contact_lr_fd, 
  CASE WHEN autocomplete.quiquoi.used IS TRUE THEN TRUE ELSE FALSE END AS is_autocom 
FROM `paj-mediacol-prd-prj-datacol.BDA.BDA` M_BDA 
LEFT JOIN M_BDA.pagesLR P_LR 
LEFT JOIN P_LR.etabs AS E_LR 
LEFT JOIn E_LR.clics AS C_LR 
LEFT JOIN E_LR.pagesFD P_FD 
LEFT JOIN P_FD.clics AS C_FD 
LEFT JOIN UNNEST(cr_recherche.rubriquesRecherche) AS RR 
WHERE 
  DATE BETWEEN DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY) AND CURRENT_DATE() 
  AND isValidatedByATInternet IS TRUE 
  AND isvalidatedbySearch IS TRUE 
  AND cr_recherche.univers = "YES" 
  AND visite.Pays = "France" 
  AND Applicatif = "Pages Jaunes" 
  AND suspicionLevel = 0 
  AND type != "PDR" 
  AND type = "LR" 
  AND origine.page = "HP" 
ORDER BY  
  2 DESC) 

SELECT  
  -- typesource, 
  -- page, 
  -- used, 
  COUNT(DISTINCT idrecherche) AS nb_rech, 
  ROUND(COUNT(DISTINCT CASE WHEN is_fd IS TRUE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lr_fd, 
  ROUND(COUNT(DISTINCT CASE WHEN is_lr IS FALSE AND is_fd IS TRUE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_fd, 
  ROUND(COUNT(DISTINCT CASE WHEN is_contact_lr_fd IS TRUE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lr_fd_contact, 
  ROUND(COUNT(DISTINCT CASE WHEN is_lr IS TRUE AND is_autocom THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lr_autocomplete, 
  ROUND(COUNT(DISTINCT CASE WHEN is_fd IS TRUE AND is_autocom THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lr_fd_autocomplete, 
  ROUND(COUNT(DISTINCT CASE WHEN is_contact_lr IS TRUE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lr_contact, 
  ROUND(COUNT(DISTINCT CASE WHEN is_contact_fd IS TRUE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_fd_contact, 
  ROUND(COUNT(DISTINCT CASE WHEN is_fd IS TRUE AND is_clic_fd IS FALSE THEN idrecherche END)/COUNT(DISTINCT idrecherche)*100,2) AS nb_rech_lost 
FROM Table_test  
-- GROUP BY 
--   1 
--   2 
-- WHERE is_lr IS FALSE 
