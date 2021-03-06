WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), CPR_EVENTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.PARENT_EVENT_ID,
		CLINICAL_EVENT.EVENT_CD,
		CE_DATE_RESULT.RESULT_DT_TM
	FROM
		CE_DATE_RESULT,
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 99985 -- Date
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 1149292336 -- CPR D/T Event Recognized
		AND CLINICAL_EVENT.EVENT_ID = CE_DATE_RESULT.EVENT_ID
		AND CE_DATE_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), CODE_START AS (
	SELECT DISTINCT
		CPR_EVENTS.ENCNTR_ID,
		MIN(CPR_EVENTS.RESULT_DT_TM) AS RESULT_DT_TM
	FROM
		CPR_EVENTS
	WHERE
		CPR_EVENTS.EVENT_CD = 1149292336 -- CPR D/T Event Recognized
	GROUP BY
		CPR_EVENTS.ENCNTR_ID
)

SELECT DISTINCT
	CODE_START.ENCNTR_ID,
	MAX(ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE) KEEP (DENSE_RANK LAST ORDER BY ORDER_DETAIL.UPDT_DT_TM) AS CODE_STATUS
FROM
	CODE_START,
	ORDER_DETAIL,
	ORDERS
WHERE
	CODE_START.ENCNTR_ID = ORDERS.ENCNTR_ID(+)
	AND ORDERS.CATALOG_CD(+) IN (
		356259461, -- Resuscitation (Code) Status
		2985032863, -- Resuscitation Status-Full Code
		2985032927 -- Resuscitation Status-DNR
	)
	AND ORDERS.CURRENT_START_DT_TM(+) < CODE_START.RESULT_DT_TM
	AND ORDERS.ORDER_ID = ORDER_DETAIL.ORDER_ID(+)
	AND ORDER_DETAIL.OE_FIELD_MEANING_ID = 229 -- RESUSCITATIONSTATUS
	AND ORDER_DETAIL.UPDT_DT_TM < CODE_START.RESULT_DT_TM
GROUP BY
	CODE_START.ENCNTR_ID