DROP TABLE IF EXISTS reservation CASCADE;
DROP TABLE IF EXISTS billet CASCADE;
DROP TABLE IF EXISTS representation CASCADE;
DROP TABLE IF EXISTS representation_externe CASCADE;
DROP TABLE IF EXISTS subvention CASCADE;
DROP TABLE IF EXISTS organisme CASCADE;
DROP TABLE IF EXISTS depense CASCADE;
DROP TABLE IF EXISTS spectacle CASCADE;
DROP TABLE IF EXISTS theatre CASCADE;
DROP TABLE IF EXISTS date_courante CASCADE;

CREATE TABLE IF NOT EXISTS date_courante (
        date_c DATE NOT NULL
);

CREATE TABLE IF NOT EXISTS theatre (
	id_theatre SERIAL PRIMARY KEY,
	nom VARCHAR(100) NOT NULL,
	pays VARCHAR(50) NOT NULL,
	departement INT NOT NULL,
	ville VARCHAR(100) NOT NULL,
	nb_places_max INT NOT NULL
);

CREATE TABLE IF NOT EXISTS spectacle (
	id_spectacle SERIAL PRIMARY KEY,
	nom VARCHAR(100) NOT NULL,
    type VARCHAR(10) NOT NULL
        CHECK  (type IN ('EXTERIEUR', 'A DOMICILE')),
	prix_spectacle numeric(10, 2) NOT NULL,
	tarif_normal numeric(10, 2) NOT NULL,
	tarif_reduit numeric(10, 2) NOT NULL
);

CREATE TABLE IF NOT EXISTS representation (
    id_representation SERIAL PRIMARY KEY,
    nb_places INT NOT NULL,
	id_theatre INT REFERENCES theatre(id_theatre),
	id_spectacle INT REFERENCES spectacle(id_spectacle),
	date_representation DATE NOT NULL,
	date_mise_en_vente DATE NOT NULL,
    politique_tarifaire VARCHAR(1) NOT NULL
        CHECK (politique_tarifaire IN ('0', '1', '2', '3')),
	UNIQUE (id_theatre, id_spectacle, date_representation)
);

CREATE TABLE IF NOT EXISTS representation_externe (
    id_representation SERIAL PRIMARY KEY,
    prix_representation numeric(10, 2) NOT NULL,
	id_theatre INT REFERENCES theatre(id_theatre),
	id_spectacle INT REFERENCES spectacle(id_spectacle),
	date_representation DATE NOT NULL,
	UNIQUE (id_theatre, id_spectacle, date_representation)
);

CREATE TABLE IF NOT EXISTS depense (
	id_depense SERIAL PRIMARY KEY,
	type_depense VARCHAR(25) NOT NULL
        CHECK(type_depense IN ('ACHAT SPECTACLE', 'COUT MISE EN SCENE')),
	montant numeric(10, 2) NOT NULL,
	date_depense DATE NOT NULL,
	id_spectacle INT REFERENCES spectacle(id_spectacle)
);

CREATE TABLE IF NOT EXISTS billet (
	id_billet SERIAL PRIMARY KEY,
	prix numeric(10, 2) NOT NULL,
	type_tarif VARCHAR(20) NOT NULL
        CHECK  (type_tarif IN ('TARIF NORMAL', 'TARIF REDUIT')),
    date_achat date NOT NULL,
	id_representation INT REFERENCES representation(id_representation)
);

CREATE TABLE IF NOT EXISTS reservation (
	id_reservation SERIAL PRIMARY KEY,
	statut VARCHAR(10) NOT NULL
		 CHECK  (statut IN ('EN ATTENTE', 'ACHETEE')) DEFAULT 'EN ATTENTE',
	type_tarif VARCHAR(20) NOT NULL
        CHECK  (type_tarif IN ('TARIF NORMAL', 'TARIF REDUIT')),
	date_reservation date NOT NULL,
	prix INT NOT NULL,
    id_representation INT REFERENCES representation(id_representation)
);

CREATE TABLE IF NOT EXISTS organisme (
	id_organisme SERIAL PRIMARY KEY,
	nom VARCHAR(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS subvention (
	id_organisme INT REFERENCES organisme(id_organisme),
	id_spectacle INT REFERENCES spectacle(id_spectacle),
	PRIMARY KEY (id_organisme, id_spectacle),
	montant numeric(10, 2) NOT NULL,
	type_action VARCHAR(30) NOT NULL
		CHECK  (type_action IN ('ACCUEIL PIECE', 'CREATION PIECE'))
);
