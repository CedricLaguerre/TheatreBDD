\echo '\n'
\echo '================================================================================================================================'
\echo '===================================================== CREATION DE FONCTIONS ===================================================='
\echo '================================================================================================================================'
\echo '\n'

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
    values (n, t, c, t_n, t_r);
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

CREATE OR REPLACE FUNCTION ajout_depense(n text, m numeric, d_d date, id_s int)
RETURNS void AS $$
BEGIN
    insert into depense (type_depense , montant , date_depense, id_spectacle)
    values (n, m, d_d, id_s);
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

CREATE OR REPLACE FUNCTION ajout_representationExterne(p numeric, id_t int, id_s int, d_r date)
RETURNS void AS $$
BEGIN
    INSERT INTO representation_externe (prix_representation, id_theatre, id_spectacle, date_representation)
    values (p, id_t, id_s, d_r);
END ;
$$
language plpgsql ;

/******************** FONCTION THEATRE ********************/

CREATE OR REPLACE FUNCTION verif_theatreExist(id_t int) RETURNS BOOLEAN AS $$
BEGIN
    PERFORM * FROM theatre WHERE id_theatre = id_t ;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'theatre n°% inexistant !', id_t ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ;
END;
$$ LANGUAGE plpgsql;

/******************** FONCTION ORGANISME ********************/

CREATE OR REPLACE FUNCTION verif_organismeExist(id_o int) RETURNS BOOLEAN AS $$
DECLARE
    org RECORD;
BEGIN
    SELECT * INTO org FROM organisme WHERE id_organisme = id_o ;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'organisme n°% inexistant !', id_o ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ;
END;
$$ LANGUAGE plpgsql;

/******************** FONCTION SUBVENTION ********************/

CREATE OR REPLACE FUNCTION verif_insertSubvention() RETURNS TRIGGER AS $$
BEGIN
    IF(verif_organismeExist(NEW.id_organisme) = TRUE AND verif_spectacleExist(NEW.id_spectacle) = TRUE) THEN
       RETURN NEW;
    ELSE
        RETURN NULL;
    END IF;
END ;
$$ LANGUAGE plpgsql ;

/******************** FONCTION SUBVENTION ********************/

CREATE OR REPLACE FUNCTION verif_insertDepense() RETURNS TRIGGER AS $$
BEGIN
    IF(verif_spectacleExist(NEW.id_spectacle) = TRUE) THEN
       RETURN NEW;
    ELSE
        RETURN NULL;
    END IF;
END ;
$$ LANGUAGE plpgsql ;

/******************** FONCTION SPECTACLE ********************/

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

CREATE OR REPLACE FUNCTION verif_notreSpec(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        spec RECORD ;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = id_s ;
    IF (spec.type = 'A DOMICILE') THEN
        RETURN TRUE ;
    ELSE
        RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_specDejaAchete(id_s int) RETURNS BOOLEAN AS $$
DECLARE
        dep RECORD ;
BEGIN
    FOR dep IN SELECT * FROM depense WHERE id_spectacle = id_s
        LOOP
            IF (dep.type_depense = 'ACHAT SPECTACLE') THEN
                RETURN TRUE ;
            END IF ;
        END LOOP ;
        RETURN FALSE;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION achat_spec(id_s int) RETURNS void AS $$
DECLARE
        spe RECORD ;
        newDate RECORD ;
BEGIN
    SELECT * INTO spe FROM spectacle WHERE id_spectacle = id_s ;
    SELECT * INTO newDate FROM date_courante LIMIT 1 ;
    IF (verif_specDejaAchete(id_s) = FALSE) THEN
        IF (verif_notreSpec(id_s) = FALSE) THEN
            IF (verif_spectacleExist(id_s) = TRUE) THEN
                RAISE NOTICE 'Le spectacle % a bien ete achete par la compagnie', spe.nom ;
                    INSERT INTO depense (type_depense, montant, date_depense, id_spectacle)
                        VALUES ('ACHAT SPECTACLE', spe.prix_spectacle, newDate.date_c, id_s) ;

            END IF;
        ELSE
            RAISE NOTICE 'Le spectacle % appartient deja a la compagnie, impossible d acheter !!!', spe.nom ;
        END IF ;
    ELSE
        RAISE NOTICE 'Le spectacle % a deja ete achete par la compagnie, impossible d acheter !!!', spe.nom ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_spectacleExist(id_s int) RETURNS BOOLEAN AS $$
DECLARE
    spec RECORD ;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = id_s ;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'spectacle inexistant !';
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ;
END;
$$ LANGUAGE plpgsql;

/******************** FONCTIONS REPRESENTATION ********************/

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
    SELECT * INTO newDate FROM date_courante ;
    SELECT * INTO thea FROM theatre WHERE id_theatre = NEW.id_theatre ;
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
    IF(verif_theatreExist(NEW.id_theatre) = FALSE) THEN
        RAISE NOTICE 'Theatre non existant !';
        RETURN NULL;
    ELSIF(verif_spectacleExist(NEW.id_spectacle) = FALSE) THEN
        RAISE NOTICE 'Spectacle non existant !';
        RETURN NULL;
    ELSIF(verif_capacitePlace(thea.nb_places_max, NEW.nb_places) = FALSE) THEN
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

CREATE OR REPLACE FUNCTION verif_representationExist(id_r int) RETURNS BOOLEAN AS $$
BEGIN
    SELECT * FROM representation WHERE id_representation = id_r ;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'representation n°% inexistant !', id_r ;
        RETURN FALSE;
    ELSE
        RETURN TRUE ;
    END IF ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_representationExt() RETURNS TRIGGER AS $$
DECLARE
    spec RECORD;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
    IF (verif_notreSpec(NEW.id_spectacle) = TRUE) THEN
        RETURN NEW ;
    ELSE
        RAISE NOTICE 'Representation impossible a lexterieur car ne nous appartient pas' ;
        RETURN NULL ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

/******************** FONCTIONS RESERVATION EXTERNE ********************/

CREATE OR REPLACE FUNCTION tournee_compagnie(id_s int) RETURNS TABLE(nom_spectacle varchar, pays varchar, dep int, ville varchar) AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    newDate RECORD ;
BEGIN
    SELECT * INTO newDate FROM date_courante LIMIT 1;
    RETURN QUERY SELECT DISTINCT spectacle.nom, theatre.pays, theatre.departement, theatre.ville FROM representation_externe
        INNER JOIN spectacle ON representation_externe.id_spectacle = spectacle.id_spectacle
        INNER JOIN theatre ON representation_externe.id_theatre = theatre.id_theatre
        WHERE (representation_externe.id_spectacle = id_s AND representation_externe.date_representation < newDate.date_c);

END;
$$ LANGUAGE plpgsql;

/******************** FONCTIONS BILLET ET RESERVATION ********************/

CREATE OR REPLACE FUNCTION verif_BilletDispo(nb_p int, id_r int) RETURNS BOOLEAN AS $$
BEGIN
    IF (nb_p < id_r) THEN
        RETURN TRUE ;
    ELSE
        RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_dateAchatBillet(d_a date, d_r date) RETURNS BOOLEAN AS $$
BEGIN
    IF (d_a < d_r) THEN
        RETURN TRUE ;
    ELSE
        RETURN FALSE ;
    END IF ;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION verif_insertBillet() RETURNS TRIGGER AS $$
DECLARE
    newDate RECORD ;
    repr RECORD ;
    spec RECORD ;
    nb_place_total INT ;
    nb_billet INT ;
    nb_res INT ;
BEGIN
    SELECT * INTO newDate FROM date_courante ;
      SELECT * INTO repr FROM representation WHERE id_representation = NEW.id_representation ;
    SELECT nom INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle;
    SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = NEW.id_representation ;
    SELECT count(id_reservation) INTO nb_res FROM reservation WHERE id_representation = NEW.id_representation ;
    nb_place_total = nb_billet + nb_res ;

    IF(verif_BilletDispo(nb_place_total, repr.nb_places) = FALSE) THEN
        RAISE NOTICE 'Plus de billet disponible pour le spectacle %', spec.nom ;
        RETURN NULL;
    ELSIF(verif_dateAchatBillet(NEW.date_achat, repr.date_representation) = FALSE) THEN
        RAISE NOTICE 'Le billet est pour une representation deja passee !';
        RETURN NULL;
    ELSE
        RAISE NOTICE 'Le billet a bien ete ajoutee';
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION verif_insertReservation() RETURNS TRIGGER AS $$
DECLARE
    newDate RECORD ;
    repr RECORD ;
    spec RECORD ;
    nb_place_total INT ;
    nb_billet INT ;
    nb_res INT ;
BEGIN
    SELECT * INTO newDate FROM date_courante ;
    SELECT * INTO repr FROM representation WHERE id_representation = NEW.id_representation ;
    SELECT nom INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle;
    SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = NEW.id_representation ;
    SELECT count(id_reservation) INTO nb_res FROM reservation WHERE id_representation = NEW.id_representation ;
    nb_place_total = nb_billet + nb_res ;

    IF(verif_BilletDispo(nb_place_total, repr.nb_places) = FALSE) THEN
        RAISE NOTICE 'Plus de reservation disponible pour le spectacle %', spec.nom ;
        RETURN NULL;
    ELSIF(verif_dateAchatBillet(NEW.date_reservation, repr.date_representation) = FALSE) THEN
        RAISE NOTICE 'La reservation est pour une representation deja passee !';
        RETURN NULL;
    ELSE
        RAISE NOTICE 'La reservation a bien ete ajoutee';
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION achat_billet(id_r int, t_t text, type text) RETURNS void AS $$
DECLARE
        newDate RECORD ;
        spec RECORD ;
        repr RECORD ;
        nb_totalBillet INT ;
        _prix INT ;
        percent_30 INT ;
        percent_50 INT ;
        nb_billet INT;
        nb_res INT;
BEGIN
    SELECT * INTO newDate FROM date_courante ;
    SELECT * INTO repr FROM representation WHERE id_representation = id_r ;
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle ;
    SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = id_r ;
    SELECT count(id_reservation) INTO nb_res FROM reservation WHERE id_representation = id_r ;

    IF (t_t = 'TARIF NORMAL') THEN
        _prix = spec.tarif_normal ;
    ELSIF (t_t = 'TARIF REDUIT') THEN
        _prix = spec.tarif_reduit ;
    END IF ;

    IF (repr.politique_tarifaire = '0') THEN
        IF (type = 'ACHAT') THEN
              INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
        ELSIF (type = 'RESERVATION') THEN
              INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
        END IF ;
        ELSIF (repr.politique_tarifaire = '1') THEN
            IF (newDate.date_c <= repr.date_mise_en_vente + interval '5 days') THEN
                _prix = _prix - (_prix * 20/100) ;
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            ELSE
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            END IF ;
        ELSIF (repr.politique_tarifaire = '2') THEN
            nb_totalBillet = nb_billet + nb_res ;
            IF (newDate.date_c >= repr.date_representation - interval '15 days') THEN
                percent_50 = (50 * repr.nb_places)/100 ;
                percent_30 = (30 * repr.nb_places)/100 ;
                IF (nb_totalBillet < percent_30) THEN
                    _prix = _prix - (_prix * 50/100) ;
                ELSIF (nb_totalBillet < percent_50) THEN
                    _prix = _prix - (_prix * 30/100) ;
                END IF ;
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            ELSE
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            END IF ;
        ELSIF (repr.politique_tarifaire = '3') THEN
            nb_totalBillet = nb_billet + nb_res ;
            percent_30 = (30 * repr.nb_places)/100 ;
            IF (percent_30 > nb_totalBillet) THEN
                _prix = _prix - (_prix * 15/100) ;
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            ELSE
                IF (type = 'ACHAT') THEN
                      INSERT INTO billet (prix, type_tarif, date_achat, id_representation) values (_prix, t_t, newDate.date_c, id_r) ;
                ELSIF (type = 'RESERVATION') THEN
                    INSERT INTO reservation (type_tarif, date_reservation, prix, id_representation) values (t_t, newDate.date_c, _prix, id_r);
                END IF ;
            END IF ;
        END IF ;
    END ;
    $$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION achat_reservation(id_res int) RETURNS void AS $$
DECLARE
    repr RECORD;
    spec RECORD;
BEGIN
    SELECT * INTO repr FROM representation WHERE id_representation = id_res ;
    SELECT * INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle ;
    RAISE NOTICE 'Votre billet pour % reserve a bien ete achete', spec.nom ;
    UPDATE reservation SET statut = 'ACHETEE' WHERE id_representation = id_res;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION res_expire() RETURNS TRIGGER AS $$
DECLARE
        res RECORD ;
        repr RECORD ;
        spec RECORD ;
        newDate RECORD ;
        nb_jours INT ;
BEGIN
    SELECT * INTO newDate FROM date_courante LIMIT 1 ;
    FOR res IN
            SELECT * FROM reservation WHERE reservation.statut = 'EN ATTENTE'
            LOOP
                SELECT * INTO repr FROM representation WHERE id_representation = res.id_representation;
                SELECT * INTO spec FROM spectacle WHERE id_spectacle = repr.id_spectacle ;
                IF (newDate.date_c + integer '2' >= repr.date_representation) THEN
                    RAISE NOTICE 'La reservation n°% du spectacle % a ete supprimee pour faute de paiement !', res.id_reservation, spec.nom ;
                    DELETE FROM reservation WHERE id_reservation = res.id_reservation ;
                ELSIF (newDate.date_c + integer '3' = repr.date_representation) THEN
                    RAISE NOTICE 'La reservation n°% du spectacle % va expirer demain, veuillez payer !', res.id_reservation, spec.nom ;
                ELSIF (newDate.date_c + integer '5' > repr.date_representation) THEN
                    nb_jours := repr.date_representation - integer '2' - newDate.date_c ;
                    RAISE NOTICE ' Il vous reste % jours pour payer la reservation n°% du spectacle % !', nb_jours, res.id_reservation, spec.nom ;
                END IF;
            END LOOP;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

/******************** HISTORIQUE ********************/

CREATE OR REPLACE FUNCTION historique_depenseMoisAnnee() RETURNS TABLE(mois text, depense numeric) AS $$
BEGIN
    RETURN QUERY
        SELECT EXTRACT(MONTH FROM date_depense) || '-' || EXTRACT(YEAR FROM date_depense) AS mois, sum(montant) AS total_mois FROM depense GROUP BY mois ORDER BY mois ;
END ;
$$ LANGUAGE plpgsql ;


CREATE OR REPLACE FUNCTION historique_depenseParSpectacle() RETURNS TABLE(id_s int, depense numeric) AS $$
BEGIN
    RETURN QUERY
        SELECT depense.id_spectacle AS id_spectacle, sum(montant) AS total_depense FROM depense 
        GROUP BY depense.id_spectacle ORDER BY depense.id_spectacle;
END ;
$$ LANGUAGE plpgsql ;

CREATE OR REPLACE FUNCTION historique_billetParSpectacle() RETURNS TABLE(id_r int, billet_vendu int, type_tarif text, prix int) AS $$
DECLARE
    rep RECORD ;
BEGIN
    RETURN QUERY
        SELECT id_representation, count(id_billet) AS nb_billet, type_tarif, prix FROM billet 
        INNER JOIN representation ON billet.id_representation = representation.id_representation GROUP BY billet.id_representation;
END ;
$$ LANGUAGE plpgsql ;

/******************** FONCTION RECETTE ********************/

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
    recette_reservation INT := 0 ;
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
            FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE'
                LOOP
                   recette_reservation := recette_reservation + res.prix ;
                END LOOP ;
        END LOOP ;

    FOR repEx IN SELECT * FROM representation_externe WHERE id_spectacle = spec.id_spectacle AND date_representation < newDate.date_c
        LOOP
            recette_representationEx := recette_representationEx + repEx.prix_representation ;
        END LOOP ;

    FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle
        LOOP
            recette_subvention := recette_subvention + subv.montant ;
        END LOOP ;
    recette_total := recette_billet + recette_reservation + recette_subvention + recette_representationEx ;
    RAISE NOTICE 'Les recettes totales de la compagnie pour le spectacle % sont de %', spec.nom, recette_total;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION recette_mois(id_s int, mois int, annee int) RETURNS void AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    _billet RECORD ;
    subv RECORD ;
    res RECORD ;
    recette_billet INT := 0 ;
    recette_subvention INT := 0 ;
    recette_reservation INT := 0 ;
    recette_totale INT := 0 ;
    newDate RECORD ;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE spectacle.id_spectacle = id_s;
    FOR rep IN SELECT * FROM representation WHERE EXTRACT(MONTH FROM date_representation) = mois AND EXTRACT(YEAR FROM date_representation) = annee
    LOOP
        FOR _billet IN SELECT * FROM billet WHERE id_representation = rep.id_representation
            LOOP
               recette_billet := recette_billet + _billet.prix ;
            END LOOP ;
        FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE'
            LOOP
               recette_reservation := recette_reservation + res.prix ;
            END LOOP ;
        FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle
            LOOP
                recette_subvention := recette_subvention + subv.montant ;
            END LOOP ;
    END LOOP ;
    recette_totale := recette_billet + recette_subvention + recette_reservation ;
    IF (mois < 10) THEN
        RAISE NOTICE 'Les recettes totales du spectacle % du mois % de l annee % est de %€', spec.nom, mois, annee, recette_totale;
    ELSE
        RAISE NOTICE 'Les recettes totales du spectacle % du mois % de l annee % est de %€', spec.nom, mois, annee, recette_totale;
    END IF ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION recette_annee(id_s int, annee int) RETURNS void AS $$
DECLARE
    spec RECORD ;
    rep RECORD ;
    _billet RECORD ;
    subv RECORD ;
    res RECORD ;
    recette_totale INT := 0 ;
    recette_billet INT := 0 ;
    recette_subvention INT := 0 ;
    recette_reservation INT := 0 ;
    newDate RECORD ;
BEGIN
    SELECT * INTO spec FROM spectacle WHERE spectacle.id_spectacle = id_s;
    FOR rep IN SELECT * FROM representation WHERE EXTRACT(YEAR FROM date_representation) = annee
    LOOP
        FOR _billet IN SELECT * FROM billet WHERE id_representation = rep.id_representation
            LOOP
               recette_billet := recette_billet + _billet.prix ;
            END LOOP ;
        FOR res IN SELECT * FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE'
            LOOP
               recette_reservation := recette_reservation + res.prix ;
            END LOOP ;
        FOR subv IN SELECT * FROM subvention WHERE id_spectacle = spec.id_spectacle
            LOOP
                recette_subvention := recette_subvention + subv.montant ;
            END LOOP ;
    END LOOP ;
    recette_totale := recette_billet + recette_subvention + recette_reservation ;
    RAISE NOTICE 'Les recettes totales du spectacle % de lannee % est de %', spec.nom, annee, recette_totale;
END;
$$ LANGUAGE plpgsql;


\echo '\n'
\echo '================================================================================================================================'
\echo '===================================================== CREATION DE TRIGGERS ====================================================='
\echo '================================================================================================================================'
\echo '\n'


/******************** TRIGGERS ********************/

CREATE TRIGGER insert_spectacle
BEFORE INSERT ON spectacle
FOR EACH ROW
        EXECUTE PROCEDURE verif_tarifSpectacle();

CREATE TRIGGER insert_representation
BEFORE INSERT ON representation
FOR EACH ROW
        EXECUTE PROCEDURE verif_insertRepresentation();

CREATE TRIGGER insert_billet
BEFORE INSERT ON billet
FOR EACH ROW
        EXECUTE PROCEDURE verif_insertBillet();

CREATE TRIGGER insert_reservation
BEFORE INSERT ON reservation
FOR EACH ROW
        EXECUTE PROCEDURE verif_insertReservation();

CREATE TRIGGER resa_expire
AFTER UPDATE of date_c ON date_courante
FOR EACH ROW
        EXECUTE PROCEDURE res_expire() ;

CREATE TRIGGER insert_subvention
BEFORE INSERT ON subvention
FOR EACH ROW
        EXECUTE PROCEDURE verif_insertSubvention();

CREATE TRIGGER insert_repEx
BEFORE INSERT ON representation_externe
FOR EACH ROW
    EXECUTE PROCEDURE verif_representationExt();

CREATE TRIGGER insert_depense
BEFORE INSERT ON depense
FOR EACH ROW
    EXECUTE PROCEDURE verif_insertDepense();

/*******
********
********
********/

-- CREATE OR REPLACE FUNCTION billet_venduTarif(id_b int) RETURNS void AS $$
-- DECLARE
--     _billet RECORD ;
--     rep RECORD ;
--     spec RECORD ;
--     nb_billet INT := 0 ;
--     nb_billetR INT := 0 ;
--     nb_billetN INT := 0 ;
--     nb_resN INT := 0 ;
--     nb_resR INT := 0 ;
--     nb_billetRTotal INT := 0 ;
--     nb_billetNTotal INT := 0 ;
--     nb_reservation INT := 0 ;
--     nb_billetTotal INT := 0 ;
-- BEGIN
--     SELECT * INTO _billet FROM billet WHERE billet.id_billet = id_b;
--     SELECT * INTO rep FROM representation WHERE representation.id_representation = _billet.id_representation;
--         SELECT * INTO spec FROM spectacle WHERE id_spectacle = rep.id_spectacle ;
--         SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = rep.id_representation ;
--         SELECT count(type_tarif) INTO nb_billetN FROM billet WHERE id_representation = rep.id_representation AND type_tarif = 'TARIF NORMAL' ;
--         SELECT count(type_tarif) INTO nb_billetR FROM billet WHERE id_representation = rep.id_representation AND type_tarif = 'TARIF REDUIT' ;
--         SELECT count(id_reservation) INTO nb_reservation FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' ;
--         SELECT count(type_tarif) INTO nb_resN FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF NORMAL' ;
--         SELECT count(type_tarif) INTO nb_resR FROM reservation WHERE id_representation = rep.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF REDUIT' ;
--
--         nb_billetTotal := nb_billet + nb_reservation ;
--         nb_billetRTotal := nb_billetR + nb_resR ;
--         nb_billetNTotal := nb_billetN + nb_resN ;
--
--         RAISE NOTICE 'Le nombre de billet vendu pour le spectacle % est de % dont % billets pris par reserversation (% billets tarif normal et % billets tarif reduit'
--                     , spec.nom, nb_billetTotal, nb_reservation, nb_billetRTotal, nb_billetNTotal;
-- END;
-- $$ LANGUAGE plpgsql;

-- CREATE OR REPLACE FUNCTION notifInfoB() RETURNS void AS $$
-- DECLARE
--     spec RECORD ;
-- BEGIN
--     SELECT INTO spec FROM billet
--     NATURAL JOIN representation
--     NATURAL JOIN spectacle WHERE billet.id_representation = NEW.id_representation ;
--     billet_venduTarif() ;
--     recette_spectacle(spec.id_spectacle) ;
-- END;
-- $$ LANGUAGE plpgsql;
--
--
-- CREATE OR REPLACE FUNCTION notifInfoR() RETURNS void AS $$
-- DECLARE
--     spec RECORD ;
-- BEGIN
--     SELECT INTO spec FROM reservation
--     NATURAL JOIN representation
--     NATURAL JOIN spectacle WHERE reservation.id_representation = NEW.id_representation ;
--     billet_venduTarif() ;
--     recette_spectacle(spec.id_spectacle) ;
-- END;
-- $$ LANGUAGE plpgsql;
--
-- CREATE TRIGGER after_insert_billet
-- AFTER INSERT ON billet
-- FOR EACH ROW
--         EXECUTE PROCEDURE notifInfoB();
--
-- CREATE TRIGGER after_update_reservation
-- AFTER UPDATE ON reservation
-- FOR EACH ROW
--         EXECUTE PROCEDURE notifInfoR();
--
-- CREATE OR REPLACE FUNCTION notif_nbBilletTarif() RETURNS void AS $$
-- DECLARE
--     _billet RECORD ;
--     rep RECORD ;
--     spec RECORD ;
--     nb_billet INT := 0 ;
--     nb_billetR INT := 0 ;
--     nb_billetN INT := 0 ;
--     nb_billetRTotal INT := 0 ;
--     nb_billetNTotal INT := 0 ;
--     nb_reservation INT := 0 ;
--     nb_billetTotal INT := 0 ;
--     nb_resN INT := 0 ;
--     nb_resR INT := 0 ;
-- BEGIN
--     SELECT * INTO spec FROM billet
--     NATURAL JOIN representation
--     NATURAL JOIN spectacle WHERE billet.id_representation = NEW.id_representation ;
--
--     SELECT count(id_billet) INTO nb_billet FROM billet WHERE id_representation = NEW.id_representation ;
--     SELECT count(type_tarif) INTO nb_billetN FROM billet WHERE id_representation = NEW.id_representationon AND type_tarif = 'TARIF NORMAL' ;
--     SELECT count(type_tarif) INTO nb_billetR FROM billet WHERE id_representation = NEW.id_representation AND type_tarif = 'TARIF REDUIT' ;
--     SELECT count(id-reservation) INTO nb_reservation FROM reservation WHERE NEW.id_representation AND statut = 'ACHETEE' ;
--     SELECT count(type_tarif) INTO nb_resN FROM reservation WHERE id_representation = NEW.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF NORMAL' ;
--     SELECT count(type_tarif) INTO nb_resR FROM reservation WHERE id_representation = NEW.id_representation AND statut = 'ACHETEE' AND type_tarif = 'TARIF REDUIT' ;
--
--     nb_billetTotal := nb_billet + nb_reservation ;
--     nb_billetRTotal := nb_billetR + nb_resR ;
--     nb_billetNTotal := nb_billetN + nb_resN ;
--
--     RAISE NOTICE 'Le nombre de billet vendu pour le spectacle % est de % dont % billets pris par reserversation (% billets tarif normal % billets tarif reduit'
--                 , spec.nom, nb_billetTotal, nb_reservation, nb_billetRTotal, nb_billetNTotal;
-- END;
-- $$ LANGUAGE plpgsql;
--
-- CREATE OR REPLACE FUNCTION notif_depense() RETURNS void AS $$
-- DECLARE
--     dep RECORD ;
--     spec RECORD ;
-- BEGIN
--     SELECT * INTO spec FROM spectacle WHERE id_spectacle = NEW.id_spectacle ;
--     SELECT sum(montant) INTO dep FROM depense WHERE id_spectacle = NEW.id_spectacle ;
--
--     RAISE NOTICE 'Les depenses totales du spectacle % est de %', spec.nom, dep ;
-- END;
-- $$ LANGUAGE plpgsql;
--
-- CREATE TRIGGER update_depense
-- AFTER UPDATE ON depense
-- FOR EACH ROW
--         EXECUTE PROCEDURE notif_depense();

-- CREATE OR REPLACE FUNCTION verif_reservationExist(id_res int) RETURNS BOOLEAN AS $$
-- BEGIN
--     SELECT * FROM theatre WHERE id_theatre = id_t ;
--     IF NOT FOUND THEN
--         RAISE EXCEPTION 'reservation n° inexistant !', id_res;
--         RETURN FALSE;
--     ELSE
--         RETURN TRUE ;
--     END IF ;
-- END;
-- $$ LANGUAGE plpgsql;
