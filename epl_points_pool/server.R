#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    library(rvest)
    library(dplyr)

    url = "https://www.premierleague.com/tables"
    webpage = read_html(url)
    
    # Using CSS selectors to scrape the team names
    team_names_html = html_nodes(webpage, ".long")
    team_names = html_text(team_names_html)
    
    head(team_names)
    
    
    
    # Rankings
    rankings_html = html_nodes(webpage, ".value")
    rankings = html_text(rankings_html)
    
    head(rankings)
    
    
    
    # Stats
    
    gp_html = html_nodes(webpage, "td:nth-child(4)")
    gp = html_text(gp_html)
    
    wins_html = html_nodes(webpage, "td:nth-child(5)")
    wins = html_text(wins_html)
    
    draws_html = html_nodes(webpage, "td:nth-child(6)")
    draws = html_text(draws_html)
    
    losses_html = html_nodes(webpage, "td:nth-child(7)")
    losses = html_text(losses_html)
    
    gf_html = html_nodes(webpage, "td:nth-child(8)")
    gf = html_text(gf_html)
    
    ga_html = html_nodes(webpage, "td:nth-child(9)")
    ga = html_text(ga_html)
    
    gd_html = html_nodes(webpage, "td:nth-child(10)")
    gd = html_text(gd_html)
    
    points_html = html_nodes(webpage, "td:nth-child(11)")
    points = html_text(points_html)
    
    
    
    
    
    team_rankings = data.frame(Rank = rankings, Team = team_names)
    
    
    
    table = data.frame(rank = rankings,
                       team = team_names,
                       games_played = gp,
                       wins = wins,
                       losses = losses,
                       draws = draws,
                       goals_for = gf,
                       goals_against = ga,
                       goal_diff = gd,
                       points = points)
    row.names(table) = NULL
    
    # format table
    table$goal_diff <- gsub("\n", "", table$goal_diff)
    
    epl_table = table[-c(21:80),]
    
    
    epl_table = epl_table %>% 
        mutate_at(c("rank", "games_played", "wins", "losses", "draws", "goals_for", "goals_against", "goal_diff", "points"), as.character) %>%   
        mutate_at(c("rank", "games_played", "wins", "losses", "draws", "goals_for", "goals_against", "goal_diff", "points"), as.numeric) %>% 
        mutate_at(c("team"), as.character)
    
    epl_table = epl_table[order(epl_table$rank),]

    
    # draft results
    owners = c("Neo", "CB", "Casey", "Jason")
    
    rd1 = c("Manchester City", "Liverpool", "Tottenham Hotspur", "Chelsea")
    rd2 = c("Wolverhampton Wanderers", "Leicester City", "Arsenal", "Manchester United")
    rd3 = c("Everton", "Watford", "West Ham United", "Crystal Palace")
    rd4 = c("Burnley", "Norwich City", "Southampton", "Bournemouth")
    rd5 = c("Newcastle United", "Brighton and Hove Albion", "Aston Villa", "Sheffield United")
    
    owner_teams = data.frame(owner = owners,
                             round_1 = rd1,
                             round_2 = rd2,
                             round_3 = rd3,
                             round_4 = rd4,
                             round_5 = rd5)
    library(tidyr)
    
    neo_teams = subset(owner_teams, owner == "Neo")
    neo_teams = gather(neo_teams, key = "round_drafted", value = "team", round_1, round_2, round_3, round_4, round_5)
    
    cb_teams = subset(owner_teams, owner == "CB")
    cb_teams = gather(cb_teams, key = "round_drafted", value = "team", round_1, round_2, round_3, round_4, round_5)
    
    casey_teams = subset(owner_teams, owner == "Casey")
    casey_teams = gather(casey_teams, key = "round_drafted", value = "team", round_1, round_2, round_3, round_4, round_5)
    
    jason_teams = subset(owner_teams, owner == "Jason")
    jason_teams = gather(jason_teams, key = "round_drafted", value = "team", round_1, round_2, round_3, round_4, round_5)
    
    
    draft_table = full_join(neo_teams, cb_teams)
    draft_table = full_join(draft_table, casey_teams)
    draft_table = full_join(draft_table, jason_teams)
    
    draft_table$team = as.character(draft_table$team)
    
    final_epl_table = full_join(epl_table, draft_table)
    
    neo_points = subset(final_epl_table, final_epl_table$owner == "Neo")
    neo_point_tally = sum(neo_points$points)
    
    cb_points = subset(final_epl_table, final_epl_table$owner == "CB")
    cb_point_tally = sum(cb_points$points)
    
    casey_points = subset(final_epl_table, final_epl_table$owner == "Casey")
    casey_point_tally = sum(casey_points$points)
    
    jason_points = subset(final_epl_table, final_epl_table$owner == "Jason")
    jason_point_tally = sum(jason_points$points)
    
    owner_points = c(neo_point_tally, cb_point_tally, casey_point_tally,  jason_point_tally)
    
    owner_table = data.frame(owner = owners,
                             points = owner_points)
    
    owner_table = owner_table[order(-owner_table$points),]
    
    row.names(owner_table) <- NULL
    
    library(DT)
    output$epl_table = DT::renderDataTable({datatable(epl_table, rownames = F, options = list(
        pageLength = 20
    ))})
    
    output$owner_table = DT::renderDataTable({datatable(owner_table, rownames=F)})
    


})
