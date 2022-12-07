#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#define LMAX 100
#define ELFMAX 3000
#define BUFSIZE 100

struct elf_s {
    int longueur;
    int tab[LMAX];
    int somme;
};
typedef struct elf_s elf;

struct ensemble_elf_s {
    int longueur;
    elf tab[ELFMAX];
};
typedef struct ensemble_elf_s ensemble_elf;



void init_elf(elf *e)
{
    e->longueur = 0;
    e->somme = 0;
}


void ajout_calorie(elf *e, int cal)
{
    assert (e->longueur < LMAX-1);
    e->longueur ++;
    e->tab[e->longueur-1] = cal;
    e->somme += cal;
}



void parse_input(ensemble_elf *elfes)
{
    elfes->longueur = 0;
    char buffer[BUFSIZE];

    int i = 0; // indice de l'elfe en cours
    init_elf(&elfes->tab[i]);

    while(fgets(buffer, BUFSIZE-1, stdin)) {
        if (buffer[0] == '\n') {
            i++;
            init_elf(&elfes->tab[i]);
            continue;
        }

        // je sais que j'ai une valeur valide
        int cal = atoi(buffer);

        ajout_calorie(&elfes->tab[i], cal);
    }
    elfes->longueur = i+1;
}


void pretty_elf(elf *e)
{
    printf("\tSomme des calories: %d\n", e->somme);
    for (int i=0; i<e->longueur; i++) {
        printf("\t%d, ", e->tab[i]);
    }
    printf("\n");
}


void pretty_elfs(ensemble_elf *elfes)
{
    printf("Les elfes:\n");
    for (int i=0; i<elfes->longueur; i++) {
        pretty_elf(&elfes->tab[i]);
    }
}

void search_max_elf(ensemble_elf *elfes)
{
    /* int elfmax = -1; */
    int elfcalmax = -1;
    int elfcalpremax = -1;
    int elfcalprepremax = -1;

    for (int i=0; i<elfes->longueur; i++) {
        int c = elfes->tab[i].somme;

        if (c > elfcalmax) {
            elfcalprepremax = elfcalpremax;
            elfcalpremax = elfcalmax;
            elfcalmax = c;
        } else if (c > elfcalpremax) {
            elfcalprepremax = elfcalpremax;
            elfcalpremax = c;
        } else if (c > elfcalprepremax) {
            elfcalprepremax = c;
        }
    }
    printf ("Elfe maximum: %d calories\n", elfcalmax);
    printf ("Deux suivants plus grands: %d, %d\n", elfcalpremax, elfcalprepremax);
    printf ("Somme des 3 plus grands %d\n", elfcalmax+elfcalpremax+elfcalprepremax);
}






int main(void)
{
    ensemble_elf elfes;

    parse_input(&elfes);

    pretty_elfs(&elfes);

    search_max_elf(&elfes);

    return 0;
}
