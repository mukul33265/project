# =====================================================================
#  STUDY HABITS & ACADEMIC STRESS - Survey Dashboard (R / Shiny)
#  Run:  shiny::runApp("app.R")
#  Packages (once): install.packages(c("shiny", "ggplot2", "dplyr"))
# =====================================================================

library(shiny)
library(ggplot2)
library(dplyr)

N <- 24  # total responses

# ------------------------- DATA --------------------------------------
# Counts out of 24 (derived from the Google Forms percentages).
# Edit the c(...) numbers if your real counts differ - every chart AND
# every written interpretation updates automatically.
mk <- function(id, text, short, type, opts, counts) {
  data.frame(qid = id, question = text, short = short, type = type,
             option = factor(opts, levels = opts), count = counts,
             stringsAsFactors = FALSE)
}

survey <- bind_rows(
  mk(1, "When do you usually start preparing for your semester examinations?", "Start of exam prep", "single",
     c("Throughout the semester", "One month before", "Two weeks before", "One week before", "Just 1-2 days before"),
     c(1, 2, 10, 9, 2)),
  mk(2, "What is your preferred method of learning?", "Preferred learning method", "multi",
     c("Classroom lectures", "YouTube tutorials", "Online courses", "Self-study (books/notes)", "Combination of sources"),
     c(2, 15, 1, 9, 10)),
  mk(3, "How many hours do you study daily outside classroom hours?", "Daily study hours", "single",
     c("Less than 1 hour", "1-2 hours", "2-3 hours", "3-5 hours", "More than 5 hours"),
     c(9, 8, 5, 2, 0)),
  mk(4, "When do you feel most productive while studying?", "Most productive time", "multi",
     c("Early morning", "Afternoon", "Evening", "Late night", "No fixed time"),
     c(5, 3, 7, 10, 8)),
  mk(5, "How satisfied are you with your current academic performance?", "Satisfaction with performance", "single",
     c("Very satisfied", "Satisfied", "Neutral", "Dissatisfied", "Very dissatisfied"),
     c(4, 8, 8, 2, 2)),
  mk(6, "To what extent do you think your study habits affect your academic performance?", "Habits affect performance", "single",
     c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"),
     c(7, 13, 4, 0, 0)),
  mk(7, "How frequently do you feel stressed due to academic pressure?", "Stress frequency", "single",
     c("Never", "Rarely", "Sometimes", "Often", "Very often"),
     c(2, 8, 5, 7, 2)),
  mk(8, "What is the biggest source of your academic stress?", "Biggest source of stress", "multi",
     c("Upcoming examinations", "Assignment deadlines", "Low grades / CGPA", "Parental expectations",
       "Placement & career pressure", "Competition with peers"),
     c(10, 5, 8, 7, 19, 6)),
  mk(9, "How do you usually manage academic stress?", "Stress management", "multi",
     c("Listening to music", "Sports / exercise", "Talking to friends/family", "Movies / social media",
       "Sleeping", "No specific way"),
     c(12, 4, 11, 8, 11, 5)),
  mk(10, "Do you experience difficulty sleeping during examination periods?", "Sleep difficulty in exams", "single",
     c("Always", "Often", "Sometimes", "Rarely", "Never"),
     c(3, 4, 7, 2, 8)),
  mk(11, "Do you think teachers provide sufficient support when you face academic difficulties?", "Teacher support", "single",
     c("Always", "Usually", "Sometimes", "Rarely", "Never"),
     c(4, 3, 6, 6, 5)),
  mk(12, "To what extent do college clubs and extracurricular activities affect your study time?", "Extracurricular impact", "single",
     c("Significantly", "Moderately", "Slightly", "Not at all"),
     c(2, 8, 7, 7)),
  mk(13, "How often do you find classroom lectures engaging and easy to understand?", "Lectures engaging", "single",
     c("Always", "Often", "Sometimes", "Rarely", "Never"),
     c(1, 2, 8, 8, 5)),
  mk(14, "Do you feel that teachers focus more on completing the syllabus than on ensuring students understand the concepts?", "Syllabus over understanding", "single",
     c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"),
     c(3, 13, 5, 3, 0)),
  mk(15, "How often do you feel that classroom teaching alone is insufficient to prepare you for examinations?", "Classroom teaching insufficient", "single",
     c("Always", "Often", "Sometimes", "Rarely", "Never"),
     c(8, 5, 8, 3, 0))
) %>%
  mutate(pct = round(count / N * 100, 1), qlabel = paste0("Q", qid, ". ", short))

# helpers used to write the interpretations from the live data
cnt <- function(q, idx) sum(survey$count[survey$qid == q][idx])
P   <- function(q, idx) paste0(sprintf("%.1f", cnt(q, idx) / N * 100), "%")

# ------------------------- CHARTS ------------------------------------
INDIGO <- "#4F46E5"; SOFT <- "#C7D2FE"
wrap <- function(x, w) vapply(as.character(x), function(s) paste(strwrap(s, w), collapse = "\n"), "")

plot_q <- function(id) {
  d <- survey %>% filter(qid == id)
  base <- theme_minimal(base_size = 12) +
    theme(legend.position = "none", panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(8, 14, 4, 8))
  if (d$type[1] == "multi") {
    d <- d %>% arrange(count)
    d$option <- factor(as.character(d$option), levels = as.character(d$option))
    d$hl <- ifelse(d$count == max(d$count), "top", "other")
    ggplot(d, aes(option, count, fill = hl)) +
      geom_col(width = 0.68) +
      geom_text(aes(label = paste0(pct, "%  (", count, ")")), hjust = -0.08, size = 3.9, colour = "#374151") +
      scale_x_discrete(labels = function(x) wrap(x, 28)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
      scale_fill_manual(values = c(top = INDIGO, other = SOFT)) +
      coord_flip(clip = "off") + base +
      theme(axis.text.y = element_text(size = 10.5, colour = "#374151"), axis.text.x = element_blank())
  } else {
    d$hl <- ifelse(d$count == max(d$count), "top", "other")
    ggplot(d, aes(option, count, fill = hl)) +
      geom_col(width = 0.68) +
      geom_text(aes(label = paste0(pct, "%\n(", count, ")")), vjust = -0.25, size = 3.8,
                lineheight = 0.9, colour = "#374151") +
      scale_x_discrete(labels = function(x) wrap(x, 11)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      scale_fill_manual(values = c(top = INDIGO, other = SOFT)) + base +
      theme(axis.text.x = element_text(size = 10, colour = "#374151"), axis.text.y = element_blank())
  }
}

plot_score <- function() {
  sc <- data.frame(
    item = c("Start exam preparation only 2 weeks or less before",
             "Study under 2 hours a day outside class",
             "Believe study habits affect performance",
             "Satisfied with academic performance",
             "Feel stressed at least sometimes",
             "Have trouble sleeping in exams (at least sometimes)",
             "Say classroom teaching alone is at least sometimes insufficient",
             "Agree teachers focus on syllabus over understanding",
             "Find lectures rarely or never engaging",
             "Say teacher support is rarely or never enough"),
    pct = c(cnt(1, 3:5), cnt(3, 1:2), cnt(6, 1:2), cnt(5, 1:2), cnt(7, 3:5),
            cnt(10, 1:3), cnt(15, 1:3), cnt(14, 1:2), cnt(13, 4:5), cnt(11, 4:5)) / N * 100,
    grp = c("Study habits", "Study habits", "Study habits", "Study habits",
            "Stress & wellbeing", "Stress & wellbeing",
            "Teaching & support", "Teaching & support", "Teaching & support", "Teaching & support"),
    stringsAsFactors = FALSE
  ) %>% arrange(pct)
  sc$item <- factor(sc$item, levels = sc$item)
  ggplot(sc, aes(item, pct, fill = grp)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")), hjust = -0.12, size = 4, colour = "#374151") +
    coord_flip() +
    scale_x_discrete(labels = function(x) wrap(x, 50)) +
    scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    scale_fill_manual(values = c("Study habits" = "#4F46E5", "Stress & wellbeing" = "#F59E0B",
                                 "Teaching & support" = "#10B981"), name = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid = element_blank(), axis.title = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_text(size = 11, colour = "#374151"),
          plot.background = element_rect(fill = "white", colour = NA))
}

# ------------------------- WRITTEN INTERPRETATIONS -------------------
insight <- function(headline, points, why) {
  div(class = "insight",
      div(class = "ins-h", "What we derive"),
      tags$p(class = "ins-t", HTML(headline)),
      tags$ul(lapply(points, function(p) tags$li(HTML(p)))),
      div(class = "why", HTML(paste0("<b>Why it matters:</b> ", why))))
}

qcard <- function(id, headline, points, why) {
  d <- survey[survey$qid == id, ][1, ]
  div(class = "card",
      div(class = "card-top", span(class = "qnum", paste0("Q", id)),
          if (d$type == "multi") span(class = "tag", "multiple answers allowed")),
      h4(class = "qtitle", d$question),
      plotOutput(paste0("q", id), height = "300px"),
      insight(headline, points, why))
}

kpi <- function(num, lbl) div(class = "card kpi", div(class = "num", num), div(class = "lbl", lbl))

cards <- list(
  `1` = qcard(1,
              paste0("Exam preparation is mostly last-minute: <b>", P(1, 3:5), "</b> start two weeks or less before exams."),
              c(paste0("Only <b>", P(1, 1), "</b> (1 student) study throughout the semester."),
                paste0("The most common start is two weeks before (<b>", P(1, 3), "</b>), then one week before (<b>", P(1, 4), "</b>)."),
                paste0("<b>", P(1, 5), "</b> begin only 1-2 days before the exam.")),
              "Late starters must squeeze the syllabus into a short window - the classic route to cramming, exam stress (Q7, Q8) and lost sleep (Q10)."),
  `2` = qcard(2,
              paste0("YouTube is the clear first choice (<b>", P(2, 2), "</b>) - ahead of every formal method."),
              c(paste0("Classroom lectures are preferred by only <b>", P(2, 1), "</b>."),
                paste0("A combination of sources (<b>", P(2, 5), "</b>) and self-study (<b>", P(2, 4), "</b>) come next."),
                paste0("Structured online courses are almost unused (<b>", P(2, 3), "</b>).")),
              "Students use free, on-demand, visual content to fill what the classroom is not giving them (compare Q13, Q14, Q15)."),
  `3` = qcard(3,
              paste0("Self-study time is short: <b>", P(3, 1:2), "</b> study under 2 hours a day outside class."),
              c(paste0("<b>", P(3, 1), "</b> study less than 1 hour daily - the single biggest group."),
                paste0("Only <b>", P(3, 4:5), "</b> study 3 hours or more; nobody studies more than 5 hours."),
                "The pattern is low daily effort with a burst near exams (see Q1)."),
              paste0("<b>", P(6, 1:2), "</b> believe habits affect performance (Q6), yet daily hours are low - an awareness-action gap, not a belief problem.")),
  `4` = qcard(4,
              paste0("Late night is the most productive time for <b>", P(4, 4), "</b>; <b>", P(4, 5), "</b> have no fixed routine."),
              c(paste0("Evening (<b>", P(4, 3), "</b>) and early morning (<b>", P(4, 1), "</b>) follow."),
                paste0("Afternoon is the least productive time (<b>", P(4, 2), "</b>) - likely when students are in class or tired."),
                "A night-heavy schedule suggests many students shift study into the hours normally used for sleep."),
              "This is a plausible contributor to sleep difficulty in exam periods (Q10)."),
  `5` = qcard(5,
              paste0("About half are satisfied (<b>", P(5, 1:2), "</b>), but only <b>", P(5, 1), "</b> are very satisfied."),
              c(paste0("<b>", P(5, 3), "</b> are neutral - neither happy nor unhappy."),
                paste0("<b>", P(5, 4:5), "</b> are dissatisfied (including very dissatisfied) - a small but real group.")),
              "Satisfaction is middling rather than poor. The large neutral group is the easiest to move with better study support."),
  `6` = qcard(6,
              paste0("Students overwhelmingly believe habits shape results: <b>", P(6, 1:2), "</b> agree."),
              c(paste0("Strongly agree <b>", P(6, 1), "</b>, agree <b>", P(6, 2), "</b>."),
                "Nobody disagrees."),
              "There is no belief barrier. Students would probably welcome study-skills help (planning, time management) - compare with Q1 and Q3, where behaviour does not match this belief."),
  `7` = qcard(7,
              paste0("Stress is common and polarised: <b>", P(7, 4:5), "</b> feel stressed often or very often, while <b>", P(7, 1:2), "</b> rarely or never do."),
              c(paste0("<b>", P(7, 3:5), "</b> feel stressed at least sometimes."),
                paste0("Only <b>", P(7, 1), "</b> never feel stressed.")),
              "A sizeable group is under frequent pressure. Q8 shows where it comes from."),
  `8` = qcard(8,
              paste0("Placement and career pressure is the biggest stressor (<b>", P(8, 5), "</b>) - far above exams (<b>", P(8, 1), "</b>)."),
              c(paste0("Low grades/CGPA <b>", P(8, 3), "</b>, parental expectations <b>", P(8, 4), "</b>, peer competition <b>", P(8, 6), "</b>."),
                paste0("Assignment deadlines are the lowest (<b>", P(8, 2), "</b>)."),
                "Stress is driven more by <i>what comes after college</i> (jobs, CGPA, family expectations) than by day-to-day coursework."),
              "Reducing exam load alone will not fix stress. Early career guidance, placement preparation and expectation management would matter more."),
  `9` = qcard(9,
              paste0("Coping is mostly passive: music (<b>", P(9, 1), "</b>), talking (<b>", P(9, 3), "</b>) and sleeping (<b>", P(9, 5), "</b>) lead; exercise is used by only <b>", P(9, 2), "</b>."),
              c(paste0("<b>", P(9, 6), "</b> (5 students) have no specific way to cope."),
                paste0("Movies/social media are used by <b>", P(9, 4), "</b>."),
                "Talking to friends or family is common, so peer and family support is a real resource."),
              "Few use active strategies such as exercise, and one in five has no strategy at all - the group most at risk of burnout. Wellness sessions and visible counselling would help."),
  `10` = qcard(10,
               paste0("Exam periods disturb sleep for most: <b>", P(10, 1:3), "</b> have trouble at least sometimes."),
               c(paste0("<b>", P(10, 1:2), "</b> have trouble always or often."),
                 paste0("Only <b>", P(10, 5), "</b> never have trouble sleeping.")),
               "Cramming (Q1), late-night study (Q4) and stress (Q7) form a plausible chain. Poor sleep hurts memory and concentration, which can cancel out the extra hours."),
  `11` = qcard(11,
               paste0("Teacher support is inconsistent: <b>", P(11, 4:5), "</b> say it is rarely or never sufficient; only <b>", P(11, 1:2), "</b> say always or usually."),
               c(paste0("<b>", P(11, 3), "</b> say sometimes."),
                 "Answers are spread across the whole scale, so the experience probably depends on the teacher."),
               "No clear majority feels well supported. Fixed doubt-clearing hours and mentoring could close this gap."),
  `12` = qcard(12,
               paste0("Clubs are not the main thief of study time: <b>", P(12, 3:4), "</b> say they affect it only slightly or not at all."),
               c(paste0("Moderate or significant impact: <b>", P(12, 1:2), "</b>."),
                 paste0("Only <b>", P(12, 1), "</b> say significantly.")),
               "Since extracurriculars are not the main cause, the low study hours in Q3 point to other reasons - motivation, planning, or weak classroom learning."),
  `13` = qcard(13,
               paste0("Lectures often fail to engage: <b>", P(13, 4:5), "</b> find them rarely or never engaging; only <b>", P(13, 1:2), "</b> often or always."),
               c(paste0("<b>", P(13, 3), "</b> say sometimes."),
                 "Engaging lectures are the exception, not the rule."),
               "This helps explain the YouTube preference in Q2: when lectures do not engage, students look elsewhere. More interactive teaching could win attention back."),
  `14` = qcard(14,
               paste0("Most feel the syllabus matters more than understanding: <b>", P(14, 1:2), "</b> agree."),
               c(paste0("Strongly agree <b>", P(14, 1), "</b>, agree <b>", P(14, 2), "</b>."),
                 paste0("Neutral <b>", P(14, 3), "</b>; disagree <b>", P(14, 4), "</b>.")),
               "A syllabus-first pace leaves students to learn concepts on their own - consistent with Q15 and Q2."),
  `15` = qcard(15,
               paste0("Classroom teaching alone is seen as insufficient: <b>", P(15, 1:2), "</b> say always or often, and <b>", P(15, 1:3), "</b> at least sometimes."),
               c("Nobody answered \"never\".",
                 paste0("Only <b>", P(15, 4), "</b> feel it is rarely insufficient.")),
               "This is the strongest signal in the survey about the teaching gap, and the reason students depend on outside resources.")
)

grid <- function(ids) div(class = "grid2", lapply(ids, function(i) cards[[as.character(i)]]))
intro <- function(title, text) div(class = "intro", h3(title), p(text))
finding <- function(num, title, evidence, meaning)
  div(class = "card finding",
      div(class = "fnum", num), h4(title),
      tags$ul(lapply(evidence, function(e) tags$li(HTML(e)))),
      div(class = "why", HTML(paste0("<b>Interpretation:</b> ", meaning))))

# ------------------------- STYLE -------------------------------------
css <- "
body{background:#F4F6FB;font-family:'Inter',system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;color:#1F2937}
.container-fluid{padding:0}
.wrap{max-width:1280px;margin:0 auto;padding:0 22px}
.hero{background:linear-gradient(120deg,#3730A3,#6D28D9 60%,#9333EA);color:#fff;padding:38px 0 34px;margin-bottom:22px}
.hero h1{margin:0 0 8px;font-weight:800;font-size:34px;letter-spacing:-.02em}
.hero p{margin:0 0 16px;font-size:16px;opacity:.92;max-width:760px}
.chips span{display:inline-block;background:rgba(255,255,255,.18);border-radius:999px;padding:5px 14px;font-size:13px;margin-right:8px}
.nav-pills{margin-bottom:22px}
.nav-pills>li>a{border-radius:999px;color:#4338CA;font-weight:600;padding:9px 20px;margin-right:6px;background:#fff;box-shadow:0 1px 4px rgba(17,24,39,.08)}
.nav-pills>li>a:hover{background:#EEF2FF;color:#3730A3}
.nav-pills>li.active>a,.nav-pills>li.active>a:focus,.nav-pills>li.active>a:hover{background:#4F46E5;color:#fff}
.card{background:#fff;border-radius:16px;box-shadow:0 2px 12px rgba(17,24,39,.07);padding:20px 22px}
.grid2{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,520px),1fr));gap:22px;margin-bottom:22px}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:16px;margin-bottom:22px}
.kpi .num{font-size:36px;font-weight:800;color:#4F46E5;line-height:1.1}
.kpi .lbl{font-size:13px;color:#6B7280;margin-top:4px}
.card-top{margin-bottom:6px}
.qnum{background:#4F46E5;color:#fff;border-radius:8px;padding:2px 10px;font-weight:700;font-size:13px}
.tag{background:#FEF3C7;color:#92400E;border-radius:8px;padding:2px 9px;font-size:12px;margin-left:8px}
.qtitle{font-size:16px;font-weight:700;margin:8px 0 6px;line-height:1.35}
.insight{background:#F5F3FF;border-left:4px solid #7C3AED;border-radius:10px;padding:14px 16px;margin-top:12px}
.ins-h{font-size:11px;letter-spacing:.09em;text-transform:uppercase;color:#6D28D9;font-weight:800}
.ins-t{font-weight:600;margin:6px 0 4px;font-size:14.5px;line-height:1.4}
.insight ul,.finding ul{padding-left:18px;margin:6px 0;color:#374151;font-size:13.5px;line-height:1.5}
.why{font-size:13px;color:#4B5563;background:#fff;border-radius:8px;padding:9px 11px;margin-top:8px;line-height:1.45}
.intro{margin-bottom:18px}
.intro h3{font-weight:800;margin:0 0 4px}
.intro p{color:#4B5563;font-size:15px;max-width:900px}
.flow{display:flex;flex-wrap:wrap;gap:8px;margin:10px 0 4px}
.step{flex:1;min-width:150px;background:#EEF2FF;border-radius:12px;padding:12px;text-align:center}
.step b{display:block;font-size:22px;color:#4F46E5}
.step span{font-size:12.5px;color:#4B5563}
.arrow{align-self:center;font-size:24px;color:#7C3AED}
.finding .fnum{width:34px;height:34px;border-radius:50%;background:#4F46E5;color:#fff;font-weight:800;display:flex;align-items:center;justify-content:center;margin-bottom:8px}
.finding h4{font-weight:700;margin:0 0 6px}
.reco li{margin-bottom:6px}
.footer{color:#9CA3AF;font-size:12.5px;text-align:center;padding:28px 0}
.shiny-output-error-validation{color:#B45309;font-weight:600}
"

# ------------------------- UI ----------------------------------------
ui <- fluidPage(
  title = "Study Habits & Academic Stress",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"),
    tags$style(HTML(css))
  ),
  div(class = "hero", div(class = "wrap",
                          h1("Study Habits & Academic Stress"),
                          p("What 24 college students told us about how they prepare for exams, how they cope with pressure, and how they see classroom teaching."),
                          div(class = "chips", span("24 responses"), span("15 questions"), span("Google Forms survey"))
  )),
  div(class = "wrap",
      tabsetPanel(id = "tabs", type = "pills",
                  
                  tabPanel("Overview",
                           div(class = "kpis",
                               kpi(P(8, 5), "name placement & career pressure as a source of stress"),
                               kpi(P(2, 2), "prefer YouTube tutorials for learning"),
                               kpi(P(1, 3:5), "start exam preparation two weeks or less before"),
                               kpi(P(14, 1:2), "say teachers focus on the syllabus, not understanding"),
                               kpi(P(10, 1:3), "have trouble sleeping during exams (at least sometimes)")
                           ),
                           div(class = "card", style = "margin-bottom:22px",
                               h4(class = "qtitle", "The big picture: how students answered the key questions"),
                               plotOutput("score", height = "470px"),
                               insight("Read together, three themes stand out.",
                                       c(paste0("<b>Teaching gap:</b> <b>", P(15, 1:3), "</b> feel classroom teaching is at least sometimes not enough, <b>", P(14, 1:2),
                                                "</b> think the syllabus comes first, and <b>", P(13, 4:5), "</b> rarely find lectures engaging."),
                                         paste0("<b>Last-minute habits:</b> <b>", P(1, 3:5), "</b> start preparing within two weeks of exams and <b>", P(3, 1:2),
                                                "</b> study under 2 hours a day - even though <b>", P(6, 1:2), "</b> know habits matter."),
                                         paste0("<b>Stress and sleep:</b> <b>", P(7, 3:5), "</b> feel stressed at least sometimes and <b>", P(10, 1:3),
                                                "</b> sleep badly during exams.")),
                                       "These themes connect: weak classroom learning pushes students to self-study late, which brings stress and lost sleep. See the Key findings tab.")
                           )
                  ),
                  
                  tabPanel("Study habits",
                           intro("Study habits & performance",
                                 "When students study, how long, how they learn - and how they feel about the results."),
                           grid(c(1, 3, 2, 4, 5, 6, 12))
                  ),
                  
                  tabPanel("Stress & wellbeing",
                           intro("Stress & wellbeing",
                                 "How often students feel stressed, what causes it, how they cope, and what it does to their sleep."),
                           grid(c(7, 8, 9, 10))
                  ),
                  
                  tabPanel("Teaching & support",
                           intro("Teaching & support",
                                 "How students rate classroom teaching and the help they get from teachers."),
                           grid(c(13, 14, 15, 11))
                  ),
                  
                  tabPanel("Key findings",
                           intro("What the data tells us",
                                 "Five patterns that connect the answers across questions, with the numbers behind each one."),
                           div(class = "card", style = "margin-bottom:22px",
                               h4(class = "qtitle", "The proposed 'last-minute cycle'"),
                               div(class = "flow",
                                   div(class = "step", tags$b(P(1, 3:5)), span("start preparing 2 weeks or less before")),
                                   div(class = "arrow", HTML("&rarr;")),
                                   div(class = "step", tags$b(P(3, 1:2)), span("study under 2 hours a day")),
                                   div(class = "arrow", HTML("&rarr;")),
                                   div(class = "step", tags$b(P(7, 3:5)), span("feel stressed at least sometimes")),
                                   div(class = "arrow", HTML("&rarr;")),
                                   div(class = "step", tags$b(P(10, 1:3)), span("sleep badly in exam periods"))
                               ),
                               div(class = "why", HTML("<b>Note:</b> this chain is a hypothesis built from the percentages. The survey summary cannot prove that the same students move through every step."))
                           ),
                           div(class = "grid2",
                               finding("1", "The last-minute cycle",
                                       c(paste0("<b>", P(1, 3:5), "</b> start preparation two weeks or less before exams (Q1)."),
                                         paste0("<b>", P(3, 1:2), "</b> study under 2 hours daily (Q3)."),
                                         paste0("<b>", P(7, 3:5), "</b> feel stressed at least sometimes and <b>", P(10, 1:3), "</b> sleep badly in exams (Q7, Q10).")),
                                       "Compressing study into a short window is the likely driver of exam stress and disturbed sleep."),
                               finding("2", "Students fill the teaching gap themselves",
                                       c(paste0("<b>", P(15, 1:3), "</b> say classroom teaching alone is at least sometimes insufficient; nobody says never (Q15)."),
                                         paste0("<b>", P(14, 1:2), "</b> agree teachers prioritise the syllabus (Q14); <b>", P(13, 4:5), "</b> rarely find lectures engaging (Q13)."),
                                         paste0("<b>", P(2, 2), "</b> prefer YouTube versus <b>", P(2, 1), "</b> classroom lectures (Q2).")),
                                       "Weak engagement and a syllabus-first pace push students to learn from outside sources."),
                               finding("3", "Future anxiety beats exam anxiety",
                                       c(paste0("Placement and career pressure: <b>", P(8, 5), "</b> (Q8)."),
                                         paste0("Exams: <b>", P(8, 1), "</b>; low grades/CGPA: <b>", P(8, 3), "</b>; parental expectations: <b>", P(8, 4), "</b>."),
                                         paste0("Assignment deadlines are the smallest source: <b>", P(8, 2), "</b>.")),
                                       "Stress is more about outcomes and the future than about coursework itself."),
                               finding("4", "Awareness-action gap",
                                       c(paste0("<b>", P(6, 1:2), "</b> believe habits affect performance (Q6)."),
                                         paste0("But <b>", P(3, 1:2), "</b> study under 2 hours daily and <b>", P(1, 3:5), "</b> prepare late (Q1, Q3)."),
                                         paste0("Only <b>", P(5, 1:2), "</b> are satisfied with their performance (Q5).")),
                                       "Students know what to do but are not doing it - a case for planning and time-management support."),
                               finding("5", "Passive coping and thin support",
                                       c(paste0("Exercise is used by only <b>", P(9, 2), "</b>; <b>", P(9, 6), "</b> have no strategy (Q9)."),
                                         paste0("<b>", P(11, 4:5), "</b> say teacher support is rarely or never sufficient (Q11)."),
                                         paste0("Music (<b>", P(9, 1), "</b>), sleeping and friends/family (<b>", P(9, 3), "</b>) are the main outlets.")),
                                       "Students cope mostly alone and in low-effort ways, with inconsistent help from teachers."),
                               div(class = "card",
                                   h4(class = "qtitle", "Suggestions for the college"),
                                   tags$ul(class = "reco",
                                           tags$li("Run study-planning workshops and add early internal tests so preparation starts before the last two weeks."),
                                           tags$li("Make lectures more interactive and concept-focused; share curated video playlists since students already prefer video."),
                                           tags$li("Start career guidance and placement preparation early to reduce future-related stress."),
                                           tags$li("Promote exam-time wellness: sleep habits, sports/yoga sessions, and visible counselling."),
                                           tags$li("Offer fixed doubt-clearing hours or mentoring so support is consistent.")),
                                   div(class = "why", HTML("<b>Limitations:</b> 24 students from one college, self-reported answers, and summary percentages only. Treat these as patterns to investigate, not proof of cause and effect.")))
                           )
                  ),
                  
                  tabPanel("Data", br(), div(class = "card", tableOutput("tbl")))
      ),
      div(class = "footer", "Built with R & Shiny - data from a Google Forms survey of 24 students")
  )
)

# ------------------------- SERVER ------------------------------------
server <- function(input, output, session) {
  
  for (i in 1:15) local({
    id <- i
    output[[paste0("q", id)]] <- renderPlot(plot_q(id), res = 96)
  })
  output$score <- renderPlot(plot_score(), res = 96)
  
  output$tbl <- renderTable(
    survey %>% transmute(Question = qlabel, Option = as.character(option),
                         Count = as.integer(count), `Percent` = pct),
    striped = TRUE, width = "100%")
}

shinyApp(ui, server)
