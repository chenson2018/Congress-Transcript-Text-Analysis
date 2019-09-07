import scrapy
from scrapy.selector import Selector

class CongressSpiderSpider(scrapy.Spider):
	name = 'congress_spider'
	allowed_domains = ['congress.gov']
	start_urls = ['https://www.congress.gov/congressional-record/browse-by-date']
	
	def parse(self, response):
		base_url = 'https://www.congress.gov/congressional-record/{}/browse-by-date'
		session_urls = response.xpath('//*[@id="browsebydate"]/option/@value').extract()
		
		for url in session_urls:
		
			next_url = base_url.format(url)
			yield scrapy.Request(url=next_url, callback=self.get_pdf)
			
	def get_pdf(self, response):
		base_url = 'https://congress.gov'
		pdfs = response.xpath('//td/a[@target="_blank"]/@href').extract()
		
		for pdf in pdfs:
			print(pdf+base_url)
			yield scrapy.Request(url=base_url+pdf, callback=self.save_pdf)
			
	def save_pdf(self, response):
	
		path = response.url.split('/')[-1]
		print(path)
		self.logger.info("Saving PDF %s", path)
		with open(path, 'wb') as f:
			f.write(response.body)