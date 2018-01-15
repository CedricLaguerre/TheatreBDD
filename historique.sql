\echo '========== Historique depense par periode mois/annee =========='
SELECT EXTRACT(MONTH FROM date_depense) || '-' || EXTRACT(YEAR FROM date_depense) AS mois, sum(montant) AS total_mois
	FROM depense GROUP BY mois ORDER BY mois ;
\echo '--------------------------------------'


\echo '========== Historique depense par periode spectacle =========='
SELECT spectacle.id_spectacle AS id_spectacle, spectacle.nom AS nom_spectacle, sum(montant) AS total_depense
	FROM spectacle INNER JOIN depense ON spectacle.id_spectacle = depense.id_spectacle GROUP BY spectacle.id_spectacle ORDER BY id_spectacle ;
\echo '--------------------------------------'

\echo '========== Historique billet vendu pour chaque representation =========='

SELECT representation.id_representation, count(id_billet) AS billet_vendu, count(type_tarif) AS tarif_normal
	FROM representation
	INNER JOIN billet ON representation.id_representation = billet.id_representation
	WHERE billet.type_tarif = 'TARIF NORMAL' GROUP BY representation.id_representation ORDER BY id_representation ;

SELECT representation.id_representation, count(id_billet) AS billet_vendu, count(type_tarif) AS tarif_reduit
	FROM representation
	INNER JOIN billet ON representation.id_representation = billet.id_representation
	WHERE billet.type_tarif = 'TARIF REDUIT' GROUP BY representation.id_representation ORDER BY id_representation ;

\echo '--------------------------------------'
