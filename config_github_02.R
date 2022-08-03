### Adicionando git ao projeto
usethis::use_git()
# escolha se quer fazer o commit dos arquivos.
# Se sim: Definitely ou Absolutely ou Yeah...

# escolha a opÃ§Ã£o para reiniciar o RStudio


### Configurando o github no projeto,
# Vai criar o repositÃ³rio na conta
# argumentos private e visibility pra alterar a visibilidade do repositÃ³rio
usethis::use_github()
# RepositÃ³rio criado!

## Algumas precauÃ§Ãµes
# Get a situation report on your current Git/GitHub status.
# Useful for diagnosing problems.
usethis::git_sitrep()

### Adicionando arquivos ---------------------------------
## Criar um arquivo README.md
usethis::use_readme_md()
# O arquivo serÃ¡ criado e aberto, para ser editado e salvo.
# faÃ§a o commit e Push para o GitHub


### Ignorar arquivos no git ---------------------------------
usethis::use_git_ignore(c("proj_setup/"))


### branch - merge ---------------------------------
## criar um branch
usethis::pr_init(branch = "fun1")

## criar nova funÃ§Ã£o em novo script
# Ctrl + Shift + n
# funÃ§Ã£o add1
# add1 <- funcion(x) x+1

# salve o arquivo na pasta R/ com o nome:
# 1-fun_add1.R

## modificar o readme
# escreva algo e salve o arquivo

# faÃ§a o commit das alteraÃ§Ãµes

## Para conseguir sincronizar o branch com o GitHub, Ã© preciso fazer o
# push para o GitHub usando pr_push()
# faÃ§a o push
usethis::pr_push()
# vai abrir a pÃ¡gina do GitHub

## Agora temos 2 opÃ§Ãµes:
# 1. continuar trabalhando
# 2. finalizar o trabalho do branch

## 1. continuar trabalhando no branch local, sincronizando com o GitHub
# Para isso, pode fechar a janela do browser e
# continuar trabalhando normalmente no R:
# faÃ§a modificaÃ§Ãµes, commit, push (pelo botÃ£o na aba git do RStudio)

## 2. finalizar o trabalho desse branch e juntar (merge) com o GitHub
# Quando quiser finalizar o trabalho e juntar os branches:
# faÃ§a o push para o GitHub usando a funÃ§Ã£o pr_push()
usethis::pr_push()
# vai abrir a janela do browser
# revise as modificaÃ§Ãµes nos arquivos pelo site
# clique no botÃ¢o "Create pull request"
# aceite o pull request (pr) pelo botÃ£o: Merge pull request
# apÃ³s o aceite do pull request os branches estÃ£o fundidos

# apagar o branch criado
usethis::pr_finish()
# vai mudar para o branch master e apagar o branch 'fun1'

