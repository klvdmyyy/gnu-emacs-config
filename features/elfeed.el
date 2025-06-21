;;; elfeed.el --- RSS Feeds Reader for GNU Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package elfeed
  :bind (("C-x w" . elfeed))
  :config
  (elfeed-update)                       ; Just run `elfeed-update' after loading `elfeed' package
  :custom
  (elfeed-feeds
   '(;; Investing.com (RU)
     "https://ru.investing.com/rss/302.rss"
     "https://ru.investing.com/rss/stock_Stocks.rss"
     "https://ru.investing.com/rss/stock_stock_picks.rss"
     "https://ru.investing.com/rss/stock_Indices.rss"
     "https://ru.investing.com/rss/commodities_Metals.rss"
     "https://ru.investing.com/rss/news_301.rss"
     "https://ru.investing.com/rss/news_1065.rss"
     "https://ru.investing.com/rss/news_1.rss"
     "https://ru.investing.com/rss/market_overview_investing_ideas.rss"
     "https://ru.investing.com/rss/market_overview_Opinion.rss")))

;;; elfeed.el ends here
