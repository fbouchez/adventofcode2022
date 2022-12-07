#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>


void ajout_score(int *scoreptr, char opponent, char me, bool first_part)
{
    printf("opp: %c, moi: %c\n", opponent, me);
    // X vaut 1, Y vaut 2 et Z vaut 3
    *scoreptr += me - (first_part ? 'W' : 'A'-1);

    opponent = opponent - 'A';
    me = me - (first_part ? 'X' : 'A' );

    if (opponent == me) {
        // draw
        printf("draw !\n");
        *scoreptr += 3;
    } else {

        if (((me+3) - opponent) % 3 == 1) {
            // WIN !!!!
            *scoreptr += 6;
        }
    }
    printf("score actuel: %d\n", *scoreptr);
}

char choisir_forme(char op, char me)
{
    op = op - 'A';
    switch (me) {
        case 'Y': me = op; break;
        case 'X': me = (op + 3 - 1) % 3; break;
        case 'Z': me = (op + 1) % 3; break;
    }
    me = me + 'A';
    return me;
}


int main(void)
{
    char buffer[10];
    int score = 0;
    bool first_part = false;

    while (fgets(buffer, 9, stdin)) {
        assert (strlen(buffer) == 4);

        char op = buffer[0];
        char me = buffer[2];

        if (!first_part) {
            me = choisir_forme(op, me);
        }

        ajout_score(&score, op, me, first_part);
    }



    return 0;
}
