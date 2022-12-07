#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

/* #define PREMIEREPARTIE true */
#define PREMIEREPARTIE false

// chaque 'stack' est une séquence (c'est vraiment une pile)
// on a une séquence de stacks

char* lire_ligne (void)
{
    char buffer[100];
    char * ret = fgets(buffer, 99, stdin);
    if (!ret) {
        return "\n";
    }
    return strdup(buffer);
}

typedef struct sequence_s sequence;

typedef struct cellule_s cellule;
struct cellule_s {
    union {
        char* ligne;
        char caisse;
        sequence* stack;
    };
    cellule *suivant;
};

struct sequence_s {
    cellule* tete;
};


sequence* nouvelle_sequence(void)
{
    sequence* seq = malloc(sizeof(sequence));
    seq->tete = NULL;
    return seq;
}

cellule* nouvelle_cellule(void)
{
    return malloc(sizeof(cellule));
}

void empiler_cellule (sequence* seq, cellule* cel)
{
    cel->suivant = seq->tete;
    seq->tete = cel;
}

void empiler_ligne (sequence* seq, char* ligne)
{
    cellule *new = nouvelle_cellule();
    new->ligne = ligne;
    empiler_cellule(seq, new);
}

cellule* depile_cellule (sequence* seq)
{
    assert (seq->tete);
    cellule *tmp = seq->tete;
    seq->tete = seq->tete->suivant;

    return tmp;
}


cellule* depile_n_cellules(sequence* seq, int nb)
{
    cellule* first = seq->tete;
    cellule* last = seq->tete;

    while (nb > 1) {
        last = last->suivant;
        nb--;
    }

    seq->tete = last->suivant;
    last->suivant = NULL;

    return first;
}

void empiler_n_cellules(sequence* seq, cellule* first)
{
    cellule *last = first;
    while (last->suivant) 
        last = last->suivant;

    last->suivant = seq->tete;
    seq->tete = first;
}


char* depiler_ligne (sequence* seq)
{
    cellule* tmp = depile_cellule(seq);
    char* ligne = tmp->ligne;
    free(tmp);
    return ligne;
}

char depiler_caisse (sequence* seq)
{
    cellule* tmp = depile_cellule(seq);
    char caisse = tmp->caisse;
    free(tmp);
    return caisse;
}



bool ligne_vide(char* ligne)
{
    return ligne[0] == '\n' && ligne[1] == '\0' ;
}

char caisse_suivante(char **ptr)
{
    char caisse;
    // ptr doit pointer sur le '[' d'une caisse
    //
    if (**ptr == '\0') // plus de caisse
        return '\0';

    if (**ptr == ' ')  // pas de caisse pour cette colonne
        caisse = '_';

    if (**ptr == '[') {
        caisse = *(*ptr+1);
    }

    for (int i=0; i<4; i++) {
        (*ptr)++;
        if (**ptr == '\0')
            break;
    }

    return caisse;
}

bool sequence_vide(sequence* seq)
{
    return seq->tete == NULL;
}



int analyse_nb_stacks(char* ligne)
{
    return (strlen(ligne) + 1) / 4;
}


void empiler_stack(sequence* stacks, sequence* st)
{
    cellule* cel = nouvelle_cellule();
    cel->stack = st;
    cel->suivant = stacks->tete;
    stacks->tete = cel;
}

void empiler_caisse(sequence* stack, char caisse)
{
    cellule* cel = nouvelle_cellule();
    cel->caisse = caisse;
    cel->suivant = stack->tete;
    stack->tete = cel;
}



sequence* creer_stacks(int nb)
{
    sequence* stacks = nouvelle_sequence();
    for (int i=nb; i>0; i--) {
        empiler_stack(stacks, nouvelle_sequence());
    }
    return stacks;
}


sequence* cherche_stack(sequence* stacks, int i)
{
    cellule* st = stacks->tete;

    while (i>1) {
        st = st->suivant;
        i--;
    }
    return st->stack;
}


void ajouter_caisse (sequence* stacks, int i, char caisse)
{
    sequence* sta = cherche_stack(stacks, i);
    empiler_caisse(sta, caisse);
}



sequence* parse_stacks(void)
{
    sequence* lignes = nouvelle_sequence();
    char* ligne = lire_ligne();

    while (! ligne_vide(ligne))
    {
        empiler_ligne(lignes, ligne);
        ligne = lire_ligne();
    }

    int nb_stacks = analyse_nb_stacks(depiler_ligne(lignes));

    sequence* stacks = creer_stacks(nb_stacks);

    while (! sequence_vide(lignes))
    {
        ligne = depiler_ligne(lignes);
        char* p = ligne;

        char caisse = caisse_suivante(&p);

        int i=1;
        while(caisse) {
            if (caisse != '_') {
                ajouter_caisse (stacks, i, caisse);
            }
            caisse = caisse_suivante(&p);
            i++;
        }
    }
    return stacks;
}

void pretty_cell(cellule* cel)
{
    if (!cel) return;
    pretty_cell(cel->suivant);
    printf("%c ", cel->caisse);
}

void pretty_stack(sequence* stack)
{
    cellule* cel = stack->tete;

    pretty_cell(cel);

}


void pretty_stacks(sequence* stacks)
{
    cellule* cel = stacks->tete;
    int i = 1;

    while (cel) {
        printf ("Stack %d: ", i);
        pretty_stack(cel->stack);
        puts("");
        cel = cel->suivant;
        i++;
    }
}

void deplace_n_caisses(sequence* stacks, int nb, int init, int dest)
{
    sequence* sti = cherche_stack(stacks, init);
    sequence* std = cherche_stack(stacks, dest);

    cellule* cel = depile_n_cellules(sti, nb);
    empiler_n_cellules(std, cel);
}

void deplace_caisse(sequence* stacks, int init, int dest)
{
    deplace_n_caisses(stacks, 1, init, dest);
}





void applique_ligne (sequence* stacks, char* ligne)
{
    int nb_caisses, stack_init, stack_dest;
    sscanf(ligne, "move %d from %d to %d\n", &nb_caisses, &stack_init, &stack_dest);

#if PREMIEREPARTIE
    for (int i=0; i<nb_caisses; i++) {
        deplace_caisse(stacks, stack_init, stack_dest);
    }
#else
    deplace_n_caisses(stacks, nb_caisses, stack_init, stack_dest);
#endif
}


void apply_moves(sequence* stacks)
{
    char* ligne;

    while (true) {
        ligne = lire_ligne();

        if (ligne_vide(ligne)) break;

        applique_ligne(stacks, ligne);
        printf("---------------------\n");
        pretty_stacks(stacks);
    }
}


char* cherche_dessus(sequence* stacks)
{
    char buf[50];
    int idx = 0;

    cellule* cel = stacks->tete;
    while (cel) {
        char caisse = depiler_caisse (cel->stack);
        buf[idx] = caisse;
        idx++;
        cel = cel->suivant;
    }
    buf[idx] = '\0';
    return strdup(buf);
}


int main(void)
{
    sequence* stacks = parse_stacks();

    pretty_stacks(stacks);
    apply_moves(stacks);

    printf("Caisses du dessus: %s\n", cherche_dessus(stacks));

    return 0;
}
