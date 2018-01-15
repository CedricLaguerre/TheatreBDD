
/*Modifier date_courante*/

CREATE OR REPLACE FUNCTION modif_dateCourante(newDate date) RETURNS void AS $$
BEGIN
    RAISE NOTICE 'La date est passee a: %', newDate ;
    UPDATE date_courante SET date_c = cast(newDate as date);
END ;
$$ LANGUAGE plpgsql ;


/*          FONCTION UTIL                 */

CREATE OR REPLACE FUNCTION ajout_theatre(n text, p text, d int, v text, nb int)
RETURNS void AS $$
BEGIN
	insert into theatre (nom , pays , departement , ville, nb_places_max)
	values (n, p, d, v, nb);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION ajout_spectacle(n text, t text, c numeric, t_n numeric, t_r numeric)
RETURNS void AS $$
BEGIN
	insert into spectacle (nom , type, prix_spectacle , tarif_normal, tarif_reduit)
	values (n, c, t_n, t_r);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION ajout_representation(nb_b int, id_t int, id_s int, d_r date, d_m date, p_t text)
RETURNS void AS $$
BEGIN
	INSERT INTO representation (nb_places, id_theatre , id_spectacle , date_representation, date_mise_en_vente, politique_tarifaire)
	values (nb_b, id_t, id_s, d_r, d_m, p_t);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION ajout_representationExterne( p numeric, id_t int, id_s int, d_r date)
RETURNS void AS $$
BEGIN
    INSERT INTO representation_externe (prix_representation, id_theatre, id_spectacle, date_representation)
    values (p, id_t, id_s, d_r);
END ;
$$
language plpgsql ;


CREATE OR REPLACE FUNCTION ajout_organisme(n text)
RETURNS void AS $$
BEGIN
	insert into organisme (nom)
	values (n);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION ajout_subvention(id_o int, id_s int, m numeric, t_a text)
RETURNS void AS $$
BEGIN
	insert into subvention (id_organisme, id_spectacle, montant, type_action)
	values (id_o, id_s, m, t_a);
END ;
$$
language plpgsql ;

CREATE TRIGGER insert_subvention
BEFORE INSERT ON subvention
FOR EACH ROW
        EXECUTE PROCEDURE verif_subvention(NEW.id_subvention);

CREATE OR REPLACE FUNCTION verif_subvention(id_s int) RETURNS TRIGGER AS $$
DECLARE
    sub RECORD ;
    spec RECORD ;
    newDate RECORD ;
BEGIN
    SELECT * INTO sub FROM subvention WHERE id_subvention = NEW.id_s ;
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = sub.id_spectacle ;
    SELECT * INTO newDate FROM date_courante LIMIT 1 ;
    IF(verif_notreSpec(spec.id_spectacle) = TRUE || verif_specDejaAchete(spec.id_spectacle) = TRUE) THEN
        RAISE NOTICE 'La subvention provenant de % dun montant de a ete ajoute !', sub.type_subvention, sub.montant ;
        RETURN NEW;
    ELSE
        RAISE NOTICE 'Vous ne pouvez pas subentionner ce spectacle (%)', spec.nom ;
        RETURN NULL;
    END IF;
END;
$$ LANGUAGE plpgsql; 

CREATE OR REPLACE FUNCTION recette_spectacle(id_s int) RETURNS void AS $$
DECLARE
    newDate RECORD ;
	spec RECORD ;
    rep RECORD ;
    repEx RECORD ;
    subv RECORD ;
    _billet RECORD ;
    res RECORD ;
    recette_billet INT := 0 ;
    recette_representationEx INT := 0 ;
    recette_subvention INT := 0;
    recette_reservation INT != 0 ;
    recette_total INT ;
BEGIN
    SELECT * INTO newDate FROM date_courante LIMIT 1 ;
   	SELECT * INTO spec FROM spectacle WHERE id_spectacle = id_s ;
   	FOR rep IN SELECT * FROM representation WHERE id_spectacle = spec.id_spectacle 
	    LOOP 
        	FOR _billet IN SELECT * FROM billet WHERE id_representation = rep.id_representation 
         	    LOOP 
     	           recette_billet := recette_billet + _billet.prix ;
         	    END LOOP ; 
            FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation WHERE statut = 'ACHETEE' ;
                LOOP 
                   recette_reservation := recette_reservation + res.prix ;
                END LOOP ; 
	    END LOOP ;

    FOR repEx IN SELECT * FROM representation_externe WHERE id_spectacle = spec.id_spectacle WHERE date_representation < newDate.date_c ;
        LOOP 
            recette_representationEx := recette_representationEx + repEx.prix_representation ;
        END LOOP ;

   	FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle 
	    LOOP 
		    recette_subvention := recette_subvention + subv.montant ;
	    END LOOP ;
	recette_total := recette_billet + recette_reservation + recette_subvention + recette_representationEx ;
	RAISE NOTICE 'Les recettes total de la compagnie pour le spectacle % est de %', spec.nom, recette_total;
END;
$$ LANGUAGE plpgsql;



CREATE OR REPLACE FUNCTION ajout_depense(t_d text, m numeric, d_d date, id_s int)
RETURNS void AS $$
BEGIN
	insert into depense (type_depense , montant , date_depense, id_spectacle)
	values (t_d, m, d_d, id_s);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION ajout_reservation(d_l date, id_s int)
RETURNS void AS $$
BEGIN
	insert into reservation (date_limite_paiement , id_spectacle)
	values (d_l, id_s);
END ;
$$
language plpgsql ;

CREATE OR REPLACE FUNCTION verif_tarifSpectacle() RETURNS TRIGGER AS $$
BEGIN
    IF (NEW.tarif_normal > NEW.tarif_reduit) THEN
    	RETURN NEW ;
    ELSE
        RAISE NOTICE 'Le tarif reduit de % doit être inferieur au tarif normal de %', NEW.tarif_reduit, NEW.tarif_normal ;
    	RETURN NULL;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_capacitePlace(th int, sp int) RETURNS BOOLEAN AS $$
BEGIN
    IF (th < sp) THEN
    	RETURN FALSE;
    ELSE
    	RETURN TRUE;
    END IF ;
    RETURN TRUE ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_dateRep(d_r date, d_c date) RETURNS BOOLEAN AS $$
BEGIN
    IF (d_r < d_c) THEN
    	RETURN FALSE;
    ELSE
    	RETURN TRUE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_venteBillet(d_r date, d_m date, d_c date) RETURNS BOOLEAN AS $$
BEGIN
    IF (d_r < d_m) THEN
    	RETURN FALSE;
    ELSIF (d_m < d_c) THEN
        RETURN FALSE;
    ELSE
    	RETURN TRUE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_insertRepresentation() RETURNS TRIGGER AS $$
DECLARE
    thea RECORD ;
    spec RECORD ;
    newDate RECORD ;
BEGIN
    SELECT * INTO thea FROM theatre WHERE id_theatre = NEW.id_theatre ;
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
    SELECT * INTO newDate FROM date_courante LIMIT 1 ;
    IF(verif_capacitePlace(thea.nb_places_max, NEW.nb_places) = FALSE) THEN
        RAISE NOTICE 'Attention la capacite du theatre est trop petite pour le spectacle';
        RETURN NULL;
    ELSIF(verif_dateRep(NEW.date_representation, newDate.date_c) = FALSE) THEN
        RAISE NOTICE 'Attention la date de representation est deja passee';
        RETURN NULL;
    ELSIF(verif_venteBillet(NEW.date_representation, NEW.date_mise_en_vente, newDate.date_c) = FALSE) THEN
        RAISE NOTICE 'Attention la date de mise en vente est après la date de représentation ou bien avant la date courante';
        RETURN NULL;
    ELSE
        RAISE NOTICE 'La representation a bien ete ajoutee';
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER insert_spectacle
BEFORE INSERT ON spectacle
FOR EACH ROW
        EXECUTE PROCEDURE verif_tarifSpectacle();

CREATE TRIGGER insert_representation
BEFORE INSERT ON representation
FOR EACH ROW
        EXECUTE PROCEDURE verif_insertRepresentation() ;

CREATE OR REPLACE FUNCTION achat_billet(id_r int, t_t text) RETURNS BOOLEAN AS $$
DECLARE
        newDate RECORD ;
        spec RECORD ;
        repr RECORD ;
        nb_totalBillet INT ;
        _prix INT ;
        percent_30 INT ;
        percent_50 INT ;
BEGIN
	SELECT * INTO newDate FROM date_courante LIMIT 1 ;
	SELECT * INTO repr FROM representation WHERE id_representation = id_r ;
	SELECT * INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle ;
	SELECT  count(id_billet) AS nb_billet FROM billet WHERE id_representation = id_r ;
	SELECT count(id_reservation) AS nb_res FROM reservation WHERE id_representation = id_r ;

	IF (t_t = 'TARIF NORMAL') THEN
		_prix := spec.tarif_normal ;
	ELSIF (t_t = 'TARIF REDUIT') THEN
		_prix := spec.tarif_reduit ;
	END IF ;

	IF (repr.politique_tarifaire = '0') THEN
		INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
	ELSIF (repr.politique_tarifaire = '1') THEN
		IF (newDate.date_c <= repr.date_mise_en_vente + interval '5 days') THEN
			_prix := _prix - (_prix * 20/100) ;
			INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
            RAISE NOTICE 'Achat billet de type % achete', t_t ;
            RETURN TRUE ;
		END IF ;
	ELSIF (repr.politique_tarifaire = '2') THEN
		nb_totalBillet := nb_billet + nb_res ;
		IF (newDate.date_c <= repr.date_representation - interval '15 days') THEN
			percent_50 := 50/100 * repr.nb_places ;
			percent_30 := 30/100 * repr.nb_places ;
			IF (nb_totalBillet/repr.nb_places * 100 < percent_30) THEN
				_prix := _prix - (_prix * 50/100) ;
			ELSIF (nb_totalBillet/repr.nb_places * 100 < percent_50) THEN
				_prix := _prix - (_prix * 30/100) ;
			END IF ;
            RAISE NOTICE 'Achat billet de type % achete', t_t ;
			INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
            RETURN TRUE ;
		END IF ;
	ELSIF (repr.politique_tarifaire = '3') THEN
		percent_30 := 30/100 * repr.nb_places ;
		IF (percent_30 > nb_billet/repr.nb_places * 100) THEN
			_prix := _prix - (_prix * 15/100) ;
            RAISE NOTICE 'Achat billet de type % achete', t_t ;
            INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
		END IF ;
	ELSE
		RETURN FALSE ;
	END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_BilletDispo() RETURNS BOOLEAN AS $$
DECLARE
		spec RECORD ;
        repr RECORD ;
        nb_place_total INT ;
BEGIN
	SELECT  count(id_billet) AS nb_billet FROM billet WHERE id_representation = NEW.id_representation ;
	SELECT count(id_reservation) AS nb_res FROM reservation WHERE id_representation = NEW.id_representation ;
	SELECT * INTO repr FROM representation WHERE id_representation = NEW.id_representation ;
	SELECT nom INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle ;
	nb_place_total := nb_billet + nb_res ;
    IF (repr.nb_places < nb_place_total) THEN
    	RETURN TRUE ;
    ELSE
        RAISE NOTICE 'Plus de billet disponible pour le spectacle %', spec.nom ;
    	RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_prixBillet() RETURNS BOOLEAN AS $$
BEGIN
    IF (NEW.prix > 0) THEN
    	RETURN TRUE ;
    ELSE
        RAISE NOTICE 'Le prix du billet doit être superieur à 0' ;
    	RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_dateAchatBillet() RETURNS BOOLEAN AS $$
DECLARE
        repr RECORD ;
BEGIN
	SELECT * INTO repr FROM representation WHERE id_representation = NEW.id_representation ;
    IF (NEW.date_achat < repr.date_representation) THEN
    	RETURN TRUE ;
    ELSE
        RAISE NOTICE 'Le billet est pour une representation deja passee !' ;
    	RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_notreSpec(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        spec RECORD ;
BEGIN
	SELECT * INTO spec FROM spectacle WHERE id_spectacle = id_s ;
    IF (spec.type = 'A DOMICILE') THEN
    	RAISE NOTICE 'Le spectacle % appartient a notre compagnie !', spec.nom ;
    	RETURN TRUE ;
    ELSE
        RAISE NOTICE 'Le spectacle % nappartient pas a notre compagnie !', spec.nom ;
    	RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;


CREATE OR REPLACE FUNCTION verif_specADepense(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        dep RECORD ;
        aDepense BOOLEAN  ;
BEGIN
	SELECT * FROM depense WHERE id_spectacle = id_s ;
	IF SQL%NOTFOUND THEN
		RETURN FALSE ;
	ELSE
		RETURN TRUE ;
	END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_specDejaAchete(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        dep RECORD ;
        estAchete BOOLEAN := FALSE ;
BEGIN
	FOR dep IN SELECT * FROM depense WHERE id_spectacle = id_s
		LOOP
			IF (dep.type_depense = 'ACHAT SPECTACLE') THEN
				RETURN TRUE ;
			END IF ;
		END LOOP ;
END ;
$$ LANGUAGE plpgsql ;


CREATE OR REPLACE FUNCTION achat_spec(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        spe RECORD ;
        newDate RECORD ;
BEGIN
	SELECT * INTO spe FROM spectacle WHERE id_spectacle = id_s ;
	SELECT * INTO newDate FROM date_courante LIMIT 1 ;
	IF (verif_specADepense(id_s) = FALSE) THEN
		IF (verif_notreSpec = FALSE) THEN
			INSERT INTO depense (type_depense, montant, date_depense, id_spectacle) 
				VALUES ('ACHAT SPECTACLE', spe.prix_spectacle, newDate.date_c, id_s) ;
		ELSE
			INSERT INTO depense (type_depense, montant, date_depense, id_spectacle) 
			VALUES ('ACHAT SPECTACLE', 0, newDate.date_c, id_s) ;
		END IF ;
		RAISE NOTICE 'spectacle % a ete achete !' , spe.nom ;
		RETURN TRUE ;
	ELSIF (verif_specDejaAchete = FALSE) THEN
		IF (verif_notreSpec = FALSE) THEN
			INSERT INTO depense (type_depense, montant, date_depense, id_spectacle) 
				VALUES ('ACHAT SPECTACLE', spe.prix_spectacle, newDate.date_c, id_s) ; 
		ELSE
			INSERT INTO depense (type_depense, montant, date_depense, id_spectacle) 
			VALUES ('ACHAT SPECTACLE', 0, newDate.date_c, id_s) ;
		END IF ;
		RAISE NOTICE 'spectacle % a ete achete !' , spe.nom ;
		RETURN TRUE ;	
	ELSE
		RAISE NOTICE 'Le spectacle % a deja ete achete par la compagnie', spe.nom ;
		RETURN FALSE ;
	END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION res_expire() RETURNS TRIGGER AS $$ 
DECLARE 
        res RECORD ;
        spec RECORD ;
        newDate RECORD ;
        nb_jours INT ;
BEGIN
	SELECT * INTO newDate FROM date_courante LIMIT 1 ;
    FOR res IN 
            SELECT * FROM reservation 
            LOOP
            	SELECT * INTO spec FROM spectacle WHERE id_spectacle = res.id_spectacle ;
                IF (newDate.date_c + interval '2 days' > spec.date_representation - interval '5 days') THEN
                	nb_jours := spec.date_representation - interval '2 days' - newDate.date_c ;
                    RAISE NOTICE ' La reservation n°% du spectacle % à % jours pour payer son billet !', res.id_reservation, spec.nom, nb_jours ;
                ELSIF (newDate.date_c + interval '2 days' = spec.date_representation - interval '5 days') THEN
                	RAISE NOTICE 'La reservation n°% du spectacle % va expirer demain !', res.id_reservation, spec.nom ;
                ELSIF (newDate.date_c + interval '2 days' < spec.date_representation - interval '5 days') THEN
                	RAISE NOTICE 'La reservation n°% du spectacle % a ete supprimer pour faute de paiement !', res.id_reservation, spec.nom ;
                	DELETE FROM reservation WHERE id_reservation = res.id_reservation ;
                END IF;
            END LOOP;
            RETURN NEW ;
END; 
$$ LANGUAGE plpgsql;

CREATE TRIGGER res_expire
AFTER UPDATE of date_courante ON date_c
FOR EACH ROW 
        EXECUTE PROCEDURE res_expire() ;


CREATE OR REPLACE FUNCTION tournee_compagnie(id_s int) RETURNS TABLE( nom_spectacle text, pays text, dep int, ville text) AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    newDate RECORD ;
BEGIN
    SELECT * INTO newDate FROM date_courante LIMIT 1;
    RETURN QUERY SELECT DISTINCT spectacle.nom, theatre.pays, theatre.departement, theatre.ville FROM representation_externe 
        INNER JOIN spectacle ON representation_externe.id_spectacle = spectacle.id_spectacle
        INNER JOIN theatre ON representation_externe.id_theatre = theatre.id_theatre 
        WHERE representation_externe.id_spectacle = id_s AND representation_externe.date_representation < newDate.date_c ;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION recette_mois(id_s int, mois int, annee int) RETURNS void AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    recette_billet INT := 0 ;
    recette_subvention INT := 0 ;
    recette_reservation INT := 0 ;
    newDate RECORD ;
BEGIN
    FOR rep IN SELECT * FROM representation WHERE EXTRACT(MONTH FROM date_representation) = mois AND EXTRACT(YEAR FROM date_representation) = anneexxxxx
    LOOP 
        FOR _billet IN SELECT * FROM billet WHERE id_representation = rep.id_representation  
            LOOP 
               recette_billet := recette_billet + _billet.prix ;
            END LOOP ;
        FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' ;
            LOOP 
               recette_reservation := recette_reservation + res.prix ;
            END LOOP ; 
        FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle 
            LOOP 
                recette_subvention := recette_subvention + subv.montant ;
            END LOOP ;
    END LOOP ;
    recette_total := recette_billet + recette_subvention + recette_reservation ;
    IF (mois < 10) THEN
        RAISE NOTICE 'Les recettes total au mois 0% de lannee % est de %', mois, annee, recette_total;
    ELSE
        RAISE NOTICE 'Les recettes total au mois % de lannee % est de %', mois, annee, recette_total;
    END IF ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION recette_annee(mois int, annee int) RETURNS void AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    recette_billet INT := 0 ;
    recette_subvention INT := 0 ;
    recette_reservation INT := 0 ;
    newDate RECORD ;
BEGIN
    FOR rep IN SELECT * FROM representation WHERE EXTRACT(YEAR FROM date_representation) = annee
    LOOP 
        FOR _billet IN SELECT * FROM billet WHERE id_representation = rep.id_representation  
            LOOP 
               recette_billet := recette_billet + _billet.prix ;
            END LOOP ;
        FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' ;
            LOOP 
               recette_reservation := recette_reservation + res.prix ;
            END LOOP ; 
        FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle 
            LOOP 
                recette_subvention := recette_subvention + subv.montant ;
            END LOOP ;
    END LOOP ;
    recette_total := recette_billet + recette_subvention + recette_reservation ;
    RAISE NOTICE 'Les recettes total de lannee % est de %', mois, annee, recette_total;
    END IF ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION billet_venduTarif() RETURNS void AS $$
DECLARE
    _billet RECORD ;
    rep RECORD ;
    spec RECORD ;
    nb_billet INT := 0 ;
    nb_billetR INT := 0 ;
    nb_billetN INT := 0 ;
    nb_billetRTotal INT := 0 ;
    nb_billetNTotal INT := 0 ;
    nb_reservation INT := 0 ;
    nb_billetTotal INT := 0 ;
BEGIN
    FOR rep IN SELECT * FROM representation ;
    LOOP
        SELECT * INTO spec FROM spectacle WHERE id_spectacle = rep.id_spectacle ;
        SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = rep.id_representation ;
        SELECT count(type_tarif) INTO nb_billetN FROM billet WHERE id_representation = rep.id_representation AND type_tarif = 'TARIF NORMAL' ;
        SELECT count(type_tarif) INTO nb_billetR FROM billet WHERE id_representation = rep.id_representation AND type_tarif = 'TARIF REDUIT' ;
        SELECT count(id-reservation) INTO nb_reservation FROM reservation WHERE rep.id_representation AND statut = 'ACHETEE' ;
        SELECT count(type_tarif) INTO nb_resN FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF NORMAL' ;
        SELECT count(type_tarif) INTO nb_resR FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF REDUIT' ;

        nb_billetTotal := nb_billet + nb_reservation ;
        nb_billetRTotal := nb_billetR + nb_resR ;
        nb_billetNTotal := nb_billetN + nb_resN ; 

        RAISE NOTICE 'Le nombre de billet vendu pour le spectacle % est de % dont % billets pris par reserversation (% billets tarif normal % billets tarif reduit'
                    , spec.nom, nb_billetTotal, nb_reservation, nb_billetRTotal, nb_billetNTotal;
    END LOOP ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION notifInfoB() RETURNS void AS $$
DECLARE
    spec RECORD ;
BEGIN
    SELECT INTO spec FROM billet
    NATURAL JOIN representation
    NATURAL JOIN spectacle WHERE billet.id_representation = NEW.id_representation ;
    billet_venduTarif() ;
    recette_spectacle(spec.id_spectacle) ;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION notifInfoR() RETURNS void AS $$
DECLARE
    spec RECORD ;
BEGIN
    SELECT INTO spec FROM reservation
    NATURAL JOIN representation
    NATURAL JOIN spectacle WHERE reservation.id_representation = NEW.id_representation ;
    billet_venduTarif() ;
    recette_spectacle(spec.id_spectacle) ;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER insert_billet
AFTER INSERT ON billet
FOR EACH ROW
        EXECUTE PROCEDURE notifInfoB();

CREATE TRIGGER update_reservation
AFTER UPDATE ON reservation
FOR EACH ROW
        EXECUTE PROCEDURE notifInfoR();




CREATE OR REPLACE FUNCTION notif_nbBilletTarif() RETURNS void AS $$
DECLARE
    _billet RECORD ;
    rep RECORD ;
    spec RECORD ;
    nb_billet INT := 0 ;
    nb_billetR INT := 0 ;
    nb_billetN INT := 0 ;
    nb_billetRTotal INT := 0 ;
    nb_billetNTotal INT := 0 ;
    nb_reservation INT := 0 ;
    nb_billetTotal INT := 0 ;
BEGIN
    SELECT * INTO spec FROM billet 
    NATURAL JOIN representation
    NATURAL JOIN spectacle WHERE billet.id_representation = NEW.id_representation ;

    SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = NEW.id_representation ;
    SELECT count(type_tarif) INTO nb_billetN FROM billet WHERE id_representation = NEW.id_representationon WHERE type_tarif = 'TARIF NORMAL' ;
    SELECT count(type_tarif) INTO nb_billetR FROM billet WHERE id_representation = NEW.id_representation WHERE type_tarif = 'TARIF REDUIT' ;
    SELECT count(id-reservation) INTO nb_reservation FROM reservation WHERE NEW.id_representation AND statut = 'ACHETEE' ;
    SELECT count(type_tarif) INTO nb_resN FROM reservation WHERE id_representation = NEW.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF NORMAL' ;
    SELECT count(type_tarif) INTO nb_resR FROM reservation WHERE id_representation = NEW.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF REDUIT' ;

    nb_billetTotal := nb_billet + nb_reservation ;
    nb_billetRTotal := nb_billetR + nb_resR ;
    nb_billetNTotal := nb_billetN + nb_resN ;

    RAISE NOTICE 'Le nombre de billet vendu pour le spectacle % est de % dont % billets pris par reserversation (% billets tarif normal % billets tarif reduit'
                , spec.nom, nb_billetTotal, nb_reservation, nb_billetRTotal, nb_billetNTotal;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION notif_depense() RETURNS void AS $$
DECLARE
    dep RECORD ;
    spec RECORD ;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
    SELECT sum(montant) INTO dep FROM depense WHERE id_spectacle = NEW.id_spectacle ;
   
    RAISE NOTICE 'Les depenses totales du spectacle % est de %', spec.nom, dep ;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_depense
AFTER UPDATE ON depense
FOR EACH ROW
        EXECUTE PROCEDURE notif_depense();

CREATE OR REPLACE FUNCTION notif_repartitionPrix() RETURNS void AS $$
DECLARE
    _billet RECORD ;
    res RECORD ;
    rep RECORD ;
    cpt_prix INT := 0 ;
    nb_billet INT := 0 ;
    nb_res INT := 0 ;
BEGIN
    SELECT * INTO rep FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
    SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_spectacle = NEW.id_spectacle ;
    SELECT count(id_reservation) INTO nb_res FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' ;
   
    RAISE NOTICE 'Le total de billet vendu est de % du spectacle % est de %', nb_billet, spec.nom, dep ;

    FOR _billet IN SELECT * FROM representation WHERE id_representation = NEW.id_representation
        LOOP
            IF
        END LOOP ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_spectacleExist(id_s int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM spectacle WHERE id_spectacle = id_s ;
    IF NOT FOUND THEN 
        RAISE EXCEPTION 'spectacle n°% inexistant !', id_s ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ; 
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_theatreExist(id_t int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM theatre WHERE id_theatre = id_t ;
    IF NOT FOUND THEN 
        RAISE EXCEPTION 'theatre n°% inexistant !', id_t ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ; 
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_representationExist(id_r int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM representation WHERE id_representation = id_r ;
    IF NOT FOUND THEN 
        RAISE EXCEPTION 'representation n° inexistant !', id_r ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ; 
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_organismeExist(id_o int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM organisme WHERE id_organisme = id_o ;
    IF NOT FOUND THEN 
        RAISE EXCEPTION 'organisme n°% inexistant !', id_o ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ; 
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_reservationExist(id_res int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM theatre WHERE id_theatre = id_t ;
    IF NOT FOUND THEN 
        RAISE EXCEPTION 'reservation n° inexistant !', id_res;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ; 
END;
$$ LANGUAGE plpgsql;



/*Faire lachat de reservation*/

-- CREATE OR REPLACE FUNCTION historique_depenseMoisAnnee() RETURNS TABLE(mois text, depense int) AS $$
-- BEGIN
-- 	RETURN QUERY
-- 		SELECT EXTRACT(MONTH FROM date_depense) || '-' || EXTRACT(YEAR FROM date_depense) AS mois, sum(montant) AS total_mois FROM depense GROUP BY mois ORDER BY mois ;
-- END ;
-- $$ LANGUAGE plpgsql ;


-- CREATE OR REPLACE FUNCTION historique_depenseParSpectacle() RETURNS TABLE(id_s int, spec text, depense int) AS $$
-- BEGIN
-- 	RETURN QUERY
-- 		SELECT depense.id_spectacle AS id_spectacle, spectacle.nom AS nom_spectacle, sum(montant) AS total_depense FROM depense INNER JOIN spectacle ON depense.id_spectacle = spectacle.id_spectacle ;
-- END ;
-- $$ LANGUAGE plpgsql ;

-- CREATE OR REPLACE FUNCTION historique_billetVendu() RETURNS TABLE(id_r int, billet_vendu int, nb_tarifN text, nb_tarifR text, prix int) AS $$
-- DECLARE
-- 	nb_tarifReduit INT ;
-- 	nb_tarifNormal INT ;
-- BEGIN
-- 	SELECT count(type_tarif) INTO nb_tarifReduit FROM billet WHERE type_tarif = 'TARIF REDUIT' ;
-- 	SELECT count(type_tarif) INTO nb_tarifNormal FROM billet WHERE type_tarif = 'TARIF NORMAL' ;
-- 	RETURN QUERY
-- 		SELECT id_representation, count(id_billet) AS nb_billet, nb_tarifReduit, nb_tarifNormal, prix FROM billet INNER JOIN representation ON billet.id_representation = representation.id_representation GROUP BY billet.id_representation;
-- END ;
-- $$ LANGUAGE plpgsql ;
