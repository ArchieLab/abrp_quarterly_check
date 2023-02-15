-- swerb error check

WITH observers AS (
SELECT did, array_to_string(array_agg(observer), ',') AS observers
FROM swerb_observers
WHERE role = 'O'
GROUP BY did
ORDER BY did),

drivers AS 
(SELECT did, array_to_string(array_agg(observer), ',') AS drivers
FROM swerb_observers
WHERE role = 'D'
GROUP BY did
ORDER BY did)

SELECT swerb_departs_data.did,
       swerb.swid,
       swerb.date,
       swerb.time, 
       swerb.focal_grp,
       swerb.event,
       swerb.seen_grp,
       swerb_departs_data.time AS depart_time,
       observers.observers,
       drivers.drivers,
       swerb.adcode,
       adcodes.descr AS adcode_description,
       swerb.adn,
       swerb.loc,
       swerb.adtime
FROM swerb
LEFT JOIN swerb_departs_data ON swerb.did = swerb_departs_data.did
LEFT JOIN observers ON swerb.did = observers.did
LEFT JOIN drivers ON swerb.did = drivers.did
LEFT JOIN adcodes ON swerb.adcode = adcodes.adcode
WHERE  event IN ('B', 'E') AND loc IS NOT NULL
ORDER BY swerb.did, swerb.date,  swerb.time;
