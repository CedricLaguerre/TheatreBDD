\i createTable.sql
\i createTrigger.sql
\echo '\n'
\echo '================================================================================================================================'
\echo '=============================================== MODIFICATION DE LA DATE COURANTE ==============================================='
\echo '================================================================================================================================'
\echo '\n'

INSERT INTO date_courante(date_c) VALUES('2017-05-10');

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== AJOUT DE THEATRES ======================================================='
\echo '================================================================================================================================'
\echo '\n'
SELECT ajout_theatre('LE THEATRE DU CHRYSANTEME', 'FRANCE', 75013, 'PARIS', 300);
SELECT ajout_theatre('THEATRE EXTERIEUR', 'FRANCE', 92100, 'BOULOGNE', 400);
SELECT ajout_theatre('THEATRE DE LA RENAISSANCE', 'FRANCE', 75010, 'PARIS', 650);
SELECT ajout_theatre('THEATRE DE LODEON', 'FRANCE', 75006, 'PARIS', 1280);
SELECT ajout_theatre('COMEDIE FRANCAISE', 'FRANCE', 75001, 'PARIS', 2014);
SELECT ajout_theatre('THEATRE ANTOINE', 'FRANCE', 75010, 'PARIS', 780);
SELECT ajout_theatre('GRAND THEATRE', 'FRANCE', 33000, 'BORDEAUX', 1100);
SELECT ajout_theatre('SCHAUBUHNE AM LEHNINER PLATZ', 'ALLEMAGNE', 10709, 'BERLIN', 560);
SELECT ajout_theatre('TEATRO OLIMPICO', 'ITALIE', 36100, 'VICENCE', 700);
SELECT ajout_theatre('TEATRO REAL', 'ESPAGNE', 23013, 'MADRID', 1750);

SELECT * FROM theatre;
\echo '\n'
\echo '================================================================================================================================'
\echo '===================================================== AJOUT DE SPECTACLES ====================================================='
\echo '================================================================================================================================'
\echo '\n'
SELECT ajout_spectacle('AHMED SYLLA AVEC UN GRAND A', 'A DOMICILE', 2000.00, 45, 40);
SELECT ajout_spectacle('SILENCE, ON TOURNE!', 'A DOMICILE', 3000.00, 35, 30);
SELECT ajout_spectacle('TOC TOC', 'A DOMICILE', 1000.00, 37, 32);
SELECT ajout_spectacle('ABRACABRUNCH', 'A DOMICILE', 4500.00, 100, 80);
SELECT ajout_spectacle('BLANCHE GARDIN', 'A DOMICILE', 3956.50, 100, 90);
SELECT ajout_spectacle('DESPERATE HOUSEMEN', 'A DOMICILE', 6442.24, 55, 50);
SELECT ajout_spectacle('CHINOIS MARRANT', 'A DOMICILE', 3765.90, 48, 44);
SELECT ajout_spectacle('DUELS A DAVIDEJONATOWN', 'EXTERIEUR', 3500.00, 40, 35);
SELECT ajout_spectacle('MAUVAIS SPECTACLE', 'A DOMICILE', 3500.00, 30, 35);
SELECT ajout_spectacle('LE COMPTE DE BOUDERBALA 2', 'EXTERIEUR', 2500.00, 39, 22);
SELECT ajout_spectacle('LES CHORISTES', 'EXTERIEUR', 1950.00, 50, 30);
SELECT ajout_spectacle('SULIVAN OFFLINE', 'A DOMICILE', 1300.00, 25, 18);
SELECT ajout_spectacle('THE VOICE', 'EXTERIEUR', 4300.00, 29, 25);
SELECT ajout_spectacle('LE PETIT PRINCE', 'EXTERIEUR', 1100.00, 12, 7);
SELECT * FROM spectacle;

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== AJOUT D ORGANISMES ======================================================'
\echo '================================================================================================================================'
\echo '\n'

SELECT ajout_organisme('MAIRIE DE PARIS');
SELECT ajout_organisme('MECENAT PRIVE');
SELECT ajout_organisme('MINISTERE DE LA CULTURE');
SELECT ajout_organisme('ASSOCIATION DES PIECES DE THEATRE');
SELECT ajout_organisme('COLLECTIVITES TERRITORIALES');
SELECT ajout_organisme('COLLECTIVITES LOCALES');
SELECT * FROM organisme;

\echo '\n'
\echo '================================================================================================================================'
\echo '===================================================== AJOUT DE SUBVENTIONS ====================================================='
\echo '================================================================================================================================'
\echo '\n'

SELECT ajout_subvention(1, 1, 2000.00, 'CREATION PIECE');
SELECT ajout_subvention(2, 1, 3000.00, 'CREATION PIECE');
SELECT ajout_subvention(3, 1, 1290.00, 'CREATION PIECE');

SELECT ajout_subvention(4, 2, 2500.00, 'CREATION PIECE');
SELECT ajout_subvention(5, 2, 5500.00, 'CREATION PIECE');
SELECT ajout_subvention(12, 2, 1500.00, 'ACCUEIL PIECE');
SELECT ajout_subvention(7, 2, 3500.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(5, 3, 8450.00, 'CREATION PIECE');
SELECT ajout_subvention(8, 3, 1450.00, 'ACCUEIL PIECE');
SELECT ajout_subvention(11, 3, 2450.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(6, 4, 9000.00, 'CREATION PIECE');
SELECT ajout_subvention(1, 4, 4000.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(1, 5, 11000.00, 'CREATION PIECE');
SELECT ajout_subvention(13, 5, 1900.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(2, 6, 14370.00, 'CREATION PIECE');
SELECT ajout_subvention(4, 6, 4400.00, 'ACCUEIL PIECE');
SELECT ajout_subvention(5, 6, 370.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(3, 7, 3190.00, 'CREATION PIECE');
SELECT ajout_subvention(6, 7, 3300.00, 'ACCUEIL PIECE');
SELECT ajout_subvention(9, 7, 3590.00, 'ACCUEIL PIECE');

SELECT ajout_subvention(4, 8, 11780.00, 'CREATION PIECE');
SELECT ajout_subvention(5, 9, 6730.00, 'CREATION PIECE');
SELECT ajout_subvention(6, 10, 6870.00, 'CREATION PIECE');
SELECT ajout_subvention(2, 11, 5500.00, 'CREATION PIECE');
SELECT ajout_subvention(2, 12, 8690.00, 'CREATION PIECE');
SELECT ajout_subvention(2, 13, 7700.00, 'CREATION PIECE');
SELECT * FROM subvention;

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== AJOUT DE DEPENSES ======================================================='
\echo '================================================================================================================================'
\echo '\n'

SELECT achat_spec('1');
SELECT achat_spec('2');
SELECT achat_spec('3');
SELECT achat_spec('4');
SELECT achat_spec('5');
SELECT achat_spec('6');
SELECT achat_spec('7');
SELECT achat_spec('8');
SELECT achat_spec('9');
SELECT achat_spec('10');
SELECT achat_spec('11');
SELECT achat_spec('12');
SELECT achat_spec('13');

-- ajout de depenses
SELECT ajout_depense('COUT MISE EN SCENE', 100, '2017-05-14', 1) ;
SELECT ajout_depense('COUT MISE EN SCENE', 200, '2017-05-15', 1) ;
SELECT ajout_depense('COUT MISE EN SCENE', 300, '2017-06-02', 1) ;
SELECT ajout_depense('COUT MISE EN SCENE', 50, '2017-06-13', 1) ;

SELECT ajout_depense('COUT MISE EN SCENE', 650, '2017-05-11', 1);
SELECT ajout_depense('COUT MISE EN SCENE', 560, '2017-05-12', 1);
SELECT ajout_depense('COUT MISE EN SCENE', 110, '2017-05-13', 1);
SELECT ajout_depense('COUT MISE EN SCENE', 280, '2017-05-14', 1);

SELECT ajout_depense('COUT MISE EN SCENE', 150, '2017-05-11', 2);
SELECT ajout_depense('COUT MISE EN SCENE', 360, '2017-05-17', 2);
SELECT ajout_depense('COUT MISE EN SCENE',510, '2017-05-18', 2);
SELECT ajout_depense('COUT MISE EN SCENE', 380, '2017-05-19', 2);

SELECT ajout_depense('COUT MISE EN SCENE', 450, '2017-05-13', 3);
SELECT ajout_depense('COUT MISE EN SCENE', 390, '2017-05-15', 3);
SELECT ajout_depense('COUT MISE EN SCENE', 100, '2017-05-16', 3);
SELECT ajout_depense('COUT MISE EN SCENE', 300, '2017-05-18', 3);

SELECT ajout_depense('COUT MISE EN SCENE', 230, '2017-05-12', 4);
SELECT ajout_depense('COUT MISE EN SCENE', 100, '2017-05-12', 4);
SELECT ajout_depense('COUT MISE EN SCENE', 320, '2017-05-13', 4);
SELECT ajout_depense('COUT MISE EN SCENE', 340, '2017-05-14', 4);

SELECT ajout_depense('COUT MISE EN SCENE', 430, '2017-05-11', 5);
SELECT ajout_depense('COUT MISE EN SCENE', 230, '2017-05-13', 5);
SELECT ajout_depense('COUT MISE EN SCENE', 120, '2017-05-13', 5);
SELECT ajout_depense('COUT MISE EN SCENE', 90, '2017-05-14', 5);

SELECT ajout_depense('COUT MISE EN SCENE', 650, '2017-05-14', 6);
SELECT ajout_depense('COUT MISE EN SCENE', 30, '2017-05-15', 6);
SELECT ajout_depense('COUT MISE EN SCENE', 200, '2017-05-16', 6);
SELECT ajout_depense('COUT MISE EN SCENE', 120, '2017-05-16', 6);

SELECT ajout_depense('COUT MISE EN SCENE', 750, '2017-05-11', 7);
SELECT ajout_depense('COUT MISE EN SCENE', 360, '2017-05-12', 7);
SELECT ajout_depense('COUT MISE EN SCENE', 110, '2017-05-13', 7);
SELECT ajout_depense('COUT MISE EN SCENE', 280, '2017-05-14', 7);

SELECT ajout_depense('COUT MISE EN SCENE', 630, '2017-05-17', 8);
SELECT ajout_depense('COUT MISE EN SCENE', 530, '2017-05-18', 8);
SELECT ajout_depense('COUT MISE EN SCENE', 120, '2017-05-18', 8);
SELECT ajout_depense('COUT MISE EN SCENE', 220, '2017-05-19', 8);

SELECT ajout_depense('COUT MISE EN SCENE', 50, '2017-05-20', 9);
SELECT ajout_depense('COUT MISE EN SCENE', 60, '2017-05-21', 9);
SELECT ajout_depense('COUT MISE EN SCENE', 810, '2017-05-21', 9);
SELECT ajout_depense('COUT MISE EN SCENE', 280, '2017-05-22', 9);

SELECT ajout_depense('COUT MISE EN SCENE', 250, '2017-05-19', 10);
SELECT ajout_depense('COUT MISE EN SCENE', 560, '2017-05-20', 10);
SELECT ajout_depense('COUT MISE EN SCENE', 110, '2017-05-20', 10);
SELECT ajout_depense('COUT MISE EN SCENE', 280, '2017-05-21', 10);

SELECT ajout_depense('COUT MISE EN SCENE', 653, '2017-05-11', 11);
SELECT ajout_depense('COUT MISE EN SCENE', 5623, '2017-05-16', 11);
SELECT ajout_depense('COUT MISE EN SCENE', 119, '2017-05-16', 11);
SELECT ajout_depense('COUT MISE EN SCENE', 283, '2017-05-20', 11);

SELECT ajout_depense('COUT MISE EN SCENE', 290, '2017-05-11', 12);
SELECT ajout_depense('COUT MISE EN SCENE', 239, '2017-05-19', 12);
SELECT ajout_depense('COUT MISE EN SCENE', 298, '2017-05-20', 12);
SELECT ajout_depense('COUT MISE EN SCENE', 429, '2017-05-23', 12);

SELECT ajout_depense('COUT MISE EN SCENE', 423, '2017-05-22', 13);
SELECT ajout_depense('COUT MISE EN SCENE', 238, '2017-05-22', 13);
SELECT ajout_depense('COUT MISE EN SCENE', 238, '2017-05-23', 13);
SELECT ajout_depense('COUT MISE EN SCENE', 492, '2017-05-24', 13);
-- SELECT * FROM depense;

\echo '\n'
\echo '================================================================================================================================'
\echo '=================================================== AJOUT DE REPRESENTATIONS ==================================================='
\echo '================================================================================================================================'
\echo '\n'
SELECT ajout_representation(250, 1, 1, '2017-05-30', '2017-05-14', '0');
SELECT ajout_representation(250, 1, 2, '2017-05-29', '2017-05-16', '1');
SELECT ajout_representation(250, 1, 3, '2017-05-28', '2017-05-13', '1');
SELECT ajout_representation(10, 1, 4, '2017-05-30', '2017-05-12', '2');
SELECT ajout_representation(10, 1, 5, '2017-05-31', '2017-05-13', '3');
SELECT ajout_representation(100, 1, 6, '2017-05-26', '2017-05-10', '0');

SELECT ajout_representation(100, 2, 7, '2017-05-26', '2017-05-10', '0');
SELECT ajout_representation(100, 2, 8, '2017-05-26', '2017-05-10', '1');
SELECT ajout_representation(100, 2, 9, '2017-05-26', '2017-05-10', '2');
SELECT ajout_representation(100, 2, 10, '2017-05-26', '2017-05-10', '3');

SELECT ajout_representation(100, 3, 11, '2017-05-26', '2017-05-10', '0');
SELECT ajout_representation(100, 3, 12, '2017-05-26', '2017-05-10', '1');
SELECT ajout_representation(100, 3, 13, '2017-05-26', '2017-05-10', '2');

\echo '\n'
\echo '================================================================================================================================'
\echo '=============================================== AJOUT DE REPRESENTATIONS EXTERNES ==============================================='
\echo '================================================================================================================================'
\echo '\n'
SELECT ajout_representationExterne(2000.00, 2, 1, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 3, 2, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 4, 3, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 5, 4, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 6, 5, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 7, 6, '2017-05-27');
SELECT ajout_representationexterne(2000.00, 8, 8, '2017-05-27');

\echo '\n'
\echo '================================================================================================================================'
\echo '=========================================== AJOUT DE REPRESENTATIONS NON AUTORISEES ============================================'
\echo '================================================================================================================================'
\echo '\n'

-- ajouts de représentations qui ne marchent pas
SELECT ajout_representation(400, 1, 1, '2017-05-31', '2017-05-16', '0');
SELECT ajout_representation(300, 1, 2, '2017-05-25', '2017-05-26', '0');
SELECT ajout_representation(300, 1, 2, '2017-05-15', '2017-05-08', '0');
SELECT * FROM representation;

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== CHANGEMENT DE DATE ======================================================'
\echo '================================================================================================================================'
\echo '\n'

SELECT modif_dateCourante('2017-05-20');

\echo '\n'
\echo '================================================================================================================================'
\echo '======================================================= AJOUT DE BILLETS ======================================================='
\echo '================================================================================================================================'
\echo '\n'

-- ajout de billets normaux avec tarif normal et tarif réduit pour politique 0
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(1, 'TARIF NORMAL', 'ACHAT');

SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(1, 'TARIF REDUIT', 'ACHAT');

SELECT achat_billet(1, 'TARIF NORMAL', 'RESERVATION');

-- ajout de billet moins de 5 jours après la mise en vente des billets pour politique 1
SELECT achat_billet(2, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(2, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(2, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(2, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(2, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(2, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(2, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(2, 'TARIF REDUIT', 'ACHAT');

-- ajout de billet plus de 5 jours après la mise en vente des billets pour politique 1
SELECT achat_billet(3, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(3, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(3, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(3, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(3, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(3, 'TARIF REDUIT', 'ACHAT');

-- ajout de billet moins de 15 jours avant la représentation où la salle est remplie à moins de 50% avec politique 2
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');
-- ajout de plus de 30 % mais moins de 50% des billets
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');
-- ajout d'un billet qui aura moins 30%
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');
-- ajout de 50% des billets
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');
-- ajout d'un billet, ici la politique ne s'applique pas car la salle est remplie à plus de 50%
SELECT achat_billet(4, 'TARIF NORMAL', 'ACHAT');

-- ajout d'un billet à moins de 30% des billets vendus
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
-- ajout de billets pour atteindre les 30% de billets vendus
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
-- ajout d'un billet à partir des 30% de billets vendus
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');

-- ajout de billets jusqua ce qu'ils n'y en ait plus de disponible
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');
-- ici il n'y a plus de billets dispo
SELECT achat_billet(5, 'TARIF NORMAL', 'ACHAT');

-- remplissage pour chaque representation
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(6, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(7, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(7, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(7, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(7, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(7, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(7, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(7, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(7, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(7, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(7, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(7, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(8, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(8, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(8, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(8, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(8, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(8, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(10, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(10, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(10, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(10, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(10, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(10, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(11, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(11, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(11, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(11, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(11, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(11, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(11, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(11, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(12, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(12, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(12, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(12, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(12, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(12, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_billet(13, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(13, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(13, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF NORMAL', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF REDUIT', 'ACHAT');
SELECT achat_billet(13, 'TARIF NORMAL', 'RESERVATION');

SELECT achat_reservation(2);
SELECT achat_reservation(4);
SELECT achat_reservation(5);
SELECT achat_reservation(7);

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== CHANGEMENT DE DATE ======================================================'
\echo '================================================================================================================================'
\echo '\n'

SELECT modif_dateCourante('2017-05-27');

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== AJOUT BILLET PERIME ====================================================='
\echo '================================================================================================================================'
\echo '\n'

-- ajout d'un billet périmé car la représentation est deja passée
SELECT achat_billet(6, 'TARIF NORMAL', 'ACHAT');

\echo '\n'
\echo '================================================================================================================================'
\echo '====================================================== CHANGEMENT DE DATE ======================================================'
\echo '================================================================================================================================'
\echo '\n'

SELECT modif_dateCourante('2017-05-28');
SELECT * FROM billet;
SELECT * FROM reservation;

SELECT recette_spectacle(1);
SELECT recette_spectacle(5);
-- SELECT historique_depenseMoisAnnee();
-- SELECT historique_depenseParSpectacle();
-- SELECT historique_billetParSpectacle();

\echo '\n'
\echo '================================================================================================================================'
\echo '========================================================== HISTORIQUE =========================================================='
\echo '================================================================================================================================'
\echo '\n'

\i historique.sql
SELECT * FROM tournee_compagnie(1);
