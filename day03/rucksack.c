#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#define CMAX 100

/* #define PREMIEREPARTIE true */
#define PREMIEREPARTIE false


bool lire_sac_a_dos(char *buf, char **ptr)
{
    char *res = fgets(buf, CMAX-1, stdin);
    if (!res) {
        return false;
    }
    int lon = strlen(buf)-1;
#if PREMIEREPARTIE
    printf("longueur: %d\n", lon);

    buf[lon] = buf[lon/2];
    buf[lon+1] = '\0';
    buf[lon/2] = '\0';
    *ptr = buf+(lon/2)+1;
#else
    buf[lon] = '\0';
#endif
    return true;
}


void compte(char *ptr, int objets[])
{
    if (!*ptr) return;

    char l = *ptr;
    if (islower(l)) {
        int i = l - 'a';
        objets[i]++;
    } else if (isupper(l)) {
        int i = l - 'A' + 26;
        objets[i]++;
    } else {
        assert(false);
    }

    compte(ptr+1, objets);
}


int compare(int *prem, int *deux)
{
    if (*prem && *deux) return 1; // +1 pour avoir entre 1 et 52
    return 1 + compare(prem+1, deux+1);
}

int compare3(int *prem, int *deux, int *trois)
{
    if (*prem && *deux && *trois) return 1; // +1 pour avoir entre 1 et 52
    return 1 + compare3(prem+1, deux+1, trois+1);
}





// renvoie la priorité de l'objet en commun
int analyse_sac_a_dos(char *premier, char*deuxieme)
{
    int objp[52] = {0}; // compte les objets du Premier sac
    int objd[52] = {0}; // ...................  Deuxieme sac

    compte(premier, objp);
    compte(deuxieme, objd);

    int i = 'p' - 'a';
    printf("letter p %d, %d\n", objp[i], objd[i]);

    return compare(objp, objd);
}


// renvoie la priorité de l'objet en commun
int analyse_sac_a_dos3(char *premier, char*deuxieme, char*troisieme)
{
    int objp[52] = {0}; // compte les objets du Premier sac
    int objd[52] = {0}; // ...................  Deuxieme sac
    int objt[52] = {0}; // ...................  Troisieme sac

    compte(premier, objp);
    compte(deuxieme, objd);
    compte(troisieme, objt);

    return compare3(objp, objd, objt);
}




int resoudre_le_probleme(void)
{
    char buffer[CMAX];
    char *deuxieme;

#if PREMIEREPARTIE
    bool ret = lire_sac_a_dos(buffer, &deuxieme); //nouvelle ligne et separation en deux.
#else
    char buf2[CMAX];
    char buf3[CMAX];
    bool ret = lire_sac_a_dos(buffer, &deuxieme); //nouvelle ligne et pas de separation
    bool ret2 = lire_sac_a_dos(buf2, &deuxieme); //nouvelle ligne
    bool ret3 = lire_sac_a_dos(buf3, &deuxieme); //nouvelle ligne
#endif
    if (!ret) {
        return 0;
    }
#if PREMIEREPARTIE
    printf("Premiere partie '%s'    --    Deuxième partie '%s'\n", buffer, deuxieme);
#else
    assert (ret2 && ret3);
#endif

#if PREMIEREPARTIE
    int val = analyse_sac_a_dos(buffer, deuxieme);
#else
    int val = analyse_sac_a_dos3(buffer, buf2, buf3);
#endif

    return val + resoudre_le_probleme();
}


int main(void)
{

    int val = resoudre_le_probleme();
    printf("Solution: %d\n", val);


    return 0;
}
