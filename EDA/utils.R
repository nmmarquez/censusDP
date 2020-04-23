calcH <- function(DF, group = FALSE){
    
    blockEDF <- DF %>%
        group_by(name, TRACT, BLOCK, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
        mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
        summarize(E = sum(Qrt) / log(exp(1)), Pop = sum(N)) %>%
        ungroup()
    
    topEDF <- DF %>%
        group_by(name, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Pop = sum(N), Qr = N / Pop) %>%
        summarize(totalE = sum(Qr * log(1/Qr)) / log(exp(1)))
    
    indexHDF <- blockEDF %>%
        left_join(topEDF, by = "name") %>%
        group_by(name) %>%
        mutate(totalPop = sum(Pop), numer = Pop / totalPop * (totalE - E)) %>%
        summarize(H = sum(numer), totalE = first(totalE), totalPop = sum(Pop)) %>%
        mutate(H = H/totalE)
    
    tractHDF <- DF %>%
        group_by(name, TRACT, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
        mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
        summarize(tractE = sum(Qrt) / log(exp(1)), tractPop = sum(N)) %>%
        ungroup() %>%
        right_join(blockEDF, by = c("name", "TRACT")) %>% 
        group_by(name, TRACT) %>%
        mutate(tractPop = sum(Pop), numer = Pop / tractPop * (tractE - E)) %>%
        summarize(
            tractH = sum(numer), tractE = first(tractE), tractPop = sum(Pop)) %>%
        mutate(tractH = tractH/tractE)
    
    outDF <- tractHDF %>%
        left_join(indexHDF, by = "name") %>%
        mutate(tractH = ifelse(is.na(tractH), 1, tractH)) %>%
        group_by(name) %>%
        summarize(Hw = sum((tractE * tractH * tractPop) / (totalE * totalPop))) %>%
        left_join(indexHDF, by = "name") %>%
        mutate(Hb = H-Hw, Pb = 1-(Hw/H))
    
    if(group){
        
        wnwH <- DF %>%
            mutate(RACE = ifelse(RACE == "White", "White", "Non-White")) %>%
            calcH(group = FALSE) %>%
            select(name, wnwH = H, wnwTotalE = totalE)
        
        nwH <- DF %>%
            filter(RACE != "White") %>%
            calcH(group = FALSE) %>%
            select(name, nwH = H, nwTotalE = totalE)
        
        gDF <- DF %>%
            mutate(RACE2 = RACE != "White") %>%
            group_by(name) %>%
            summarize(nwQ = sum(RACE2 * value) / sum(value)) %>%
            left_join(wnwH, by = "name") %>%
            left_join(nwH, by = "name")
        
        outDF <- outDF %>%
            left_join(gDF, by = "name") %>%
            mutate(Hbg = wnwTotalE / totalE * wnwH, Pbg = Hbg / H) %>%
            mutate(Hwg = nwTotalE / totalE * nwH * nwQ) %>%
            select(-nwQ, -wnwH, -wnwTotalE, -nwH, -nwTotalE, -Hwg)
    }
    
    outDF %>%
        select(-totalPop)
}

### TESTING
# tibble(
#     RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
#     value = c(10, 10, 10, 10, 10, 10, 10, 10),
#     TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
#     BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
#     mutate(name="test") %>%
#     calcH()
# 
# tibble(
#     RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
#     value = c(10, 0, 0, 10, 10, 0, 0, 10),
#     TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
#     BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
#     mutate(name="test") %>%
#     calcH()
# 
# tibble(
#     RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
#     value = c(10, 0, 10, 0, 0, 10, 0, 10),
#     TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
#     BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
#     mutate(name="test") %>%
#     calcH()
# 
# tibble(
#     RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
#     value = c(10, 1, 10, 9, 5, 10, 5, 10),
#     TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
#     BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
#     mutate(name="test") %>%
#     calcH()