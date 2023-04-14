# Modelo DGLM
A <- dglm(log(VALOR_t)~AREA_t+BAIRRO_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t, ~AREA_t,
          family = gaussian)

glm(log(VALOR_t)~AREA_t+BAIRRO_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t,
    family = gaussian)