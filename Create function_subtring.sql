SELECT  
        two_stages_cargos.internal_reference AS "Cargo initial",
        fretlink_ops.lastname AS "Pilote de flux",
        shippers.name AS "Shipper",
        delivered_cargos_including_ops_metrics.shipper_price AS "Prix shipper - Cargo Initial",
        delivered_cargos_including_ops_metrics.carrier_price AS "Prix carrier - Cargo Initial",
        SUM(delivered_cargos_including_ops_metrics.shipper_price - delivered_cargos_including_ops_metrics.carrier_price)::FLOAT/NULLIF(delivered_cargos_including_ops_metrics.carrier_price,0) AS "Marge % - Cargo Initial",
        delivered_cargos_including_ops_metrics.status_date  AS "Date - Cargo Initial",
        cargo_details.internal_reference AS "Cargo extracost",
        cargo_details.status_date AS "Date - Cargo extracost",
        shipper_prices.total_price/100 AS "Prix shipper - Cargo extracost",        
        CASE WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%andés%' THEN 'Frais client demandés'
            WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%idés%' THEN 'Frais client validés'
        ELSE 'other' END AS statut_shipper_extracost,
        transport_offer_prices.total_price/100 AS "Prix carrier - Cargo extracost",
        CASE WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%andés%' THEN 'Frais transporteur demandés'
            WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%idés%' THEN 'Frais transporteur validés'
        ELSE 'other' END AS statut_carrier_extracost,
        SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS TRANSPORTEUR ' IN regulations_as_cargos.internal_comment) FOR 150))),
        SUM(shipper_prices.total_price/100 - transport_offer_prices.total_price/100)::FLOAT/NULLIF(transport_offer_prices.total_price/100,0) AS "Marge % - Cargo extracost",
        regulations_as_cargos.internal_comment AS "Commentaire Admin - Cargo extracost",
        SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('=== EXTRA CHARGES : ' IN regulations_as_cargos.internal_comment) + 20 
        FOR POSITION('===============' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('=== EXTRA CHARGES : ' IN regulations_as_cargos.internal_comment) FOR 150)) - 22 ) AS Nature_regul,
        load_events.event_responsible,
        unload_events.event_responsible,
        CASE WHEN load_events.event_responsible IS NULL AND unload_events.event_responsible IS NULL THEN 'Non renseigné'
             WHEN load_events.event_responsible IS NULL THEN unload_events.event_responsible
             WHEN unload_events.event_responsible IS NULL THEN load_events.event_responsible
             WHEN load_events.event_responsible IS NOT NULL AND unload_events.event_responsible IS NOT NULL THEN CONCAT(load_events.event_responsible,' and ',unload_events.event_responsible)
             ELSE 'Non renseigné' END,
        CASE WHEN (shipper_prices.total_price/100) = 0
            THEN 'Alerte'
            ELSE 'Rien' END AS "Frais Attente"
FROM cargo_details
        LEFT JOIN shipper_prices ON shipper_prices.cargo_id = cargo_details.id 
        LEFT JOIN latest_accepted_offers ON latest_accepted_offers.cargo_id = cargo_details.id 
        LEFT JOIN transport_offer_prices ON transport_offer_prices.offer_id = latest_accepted_offers.offer_id
        LEFT JOIN cargo_fee_relations ON cargo_fee_relations.cargo_id = cargo_details.id
        LEFT JOIN delivered_cargos_including_ops_metrics ON delivered_cargos_including_ops_metrics.cargo_id = best_match
        LEFT JOIN regulations_as_cargos ON regulations_as_cargos.id = cargo_details.id
        LEFT JOIN two_stages_cargos ON two_stages_cargos.id = best_match
        LEFT JOIN fretlink_ops ON fretlink_ops.id = cargo_details.followed_by
        LEFT JOIN stage_events AS load_events ON load_events.stage_key = two_stages_cargos.pickup_stage_key
        LEFT JOIN stage_events AS unload_events ON unload_events.stage_key = two_stages_cargos.delivery_stage_key
        LEFT JOIN shippers ON shippers.id = cargo_details.shipper_id

WHERE cargo_details.is_extra_cost IS TRUE 
      AND cargo_details.status_date > '2020-01-01'
      AND cargo_details.status IN ('delivered','ongoing','searching')
      AND regulations_as_cargos.internal_comment LIKE '%EXTRA%'
      [[AND {{status_date}}]]
      [[AND {{shipper}}]]
      [[AND {{lastname}}]]
      [[AND {{Reference_cargo}}]]
      [[AND CASE WHEN (shipper_prices.total_price/100) <= 5
            THEN 'A'
            ELSE 'Rien' END={{Frais_attente}}]]
      [[AND         CASE WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%andés%' THEN 'D'
            WHEN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) + 22
        FOR POSITION('€ ' IN SUBSTRING(regulations_as_cargos.internal_comment FROM POSITION('FRAIS CLIENT ' IN regulations_as_cargos.internal_comment) FOR 150))) LIKE '%idés%' THEN 'V'
        ELSE 'O' END= {{Statut_regul}}]]
      [[AND CASE WHEN regulations_as_cargos.internal_comment LIKE '"=============== EXTRA CHARGES%' THEN 'B' ELSE 'M' END={{Saisie}}]]

GROUP BY best_match,
        delivered_cargos_including_ops_metrics.shipper_price, 
        delivered_cargos_including_ops_metrics.carrier_price,
        delivered_cargos_including_ops_metrics.status_date,
        cargo_details.id,
        cargo_details.internal_reference,
        cargo_details.status_date,
        shipper_prices.total_price,
        transport_offer_prices.total_price,
        two_stages_cargos.internal_reference,
        regulations_as_cargos.internal_comment,
        fretlink_ops.lastname,
        load_events.event_responsible,
        unload_events.event_responsible,
        shippers.name
ORDER BY cargo_details.status_date DESC ,two_stages_cargos.internal_reference DESC
