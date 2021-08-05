<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:include href="CITATION.cff.xslt"/>
  <xsl:output method="text" omit-xml-declaration="yes" encoding="UTF-8"/>
  <xsl:template match="/">
    <xsl:call-template name="citation-header"/>    
references:<xsl:for-each select="bibliography/CreativeWork|bibliography/Article|bibliography/Report|bibliography/Book|bibliography/ScholarlyArticle|bibliography/BlogPosting|bibliography/TechArticle|bibliography/DigitalDocument|bibliography/PublicationIssue|bibliography/SoftwareApplication|bibliography/WebPage|bibliography/WebSite|bibliography/SoftwareSourceCode|bibliography/Comment|bibliography/QAPage|bibliography/BlogPosting|bibliography/DigitalDocument">
    - type: <xsl:choose>
    <xsl:when test="local-name()='CreativeWork'">generic</xsl:when>
    <xsl:when test="local-name()='Article'">article</xsl:when>
    <xsl:when test="local-name()='Report'">report</xsl:when>
    <xsl:when test="local-name()='Book'">book</xsl:when>
    <xsl:when test="local-name()='ScholarlyArticle'">article</xsl:when>
    <xsl:when test="local-name()='BlogPosting'">blog</xsl:when>
    <xsl:when test="local-name()='TechArticle'">article</xsl:when>
    <xsl:when test="local-name()='DigitalDocument'">data</xsl:when>
    <xsl:when test="local-name()='PublicationIssue'">article</xsl:when>
    <xsl:when test="local-name()='SoftwareApplication'">software-executable</xsl:when>
    <xsl:when test="local-name()='WebPage'">article</xsl:when>
    <xsl:when test="local-name()='WebSite'">website</xsl:when>
    <xsl:when test="local-name()='SoftwareSourceCode'">software-code</xsl:when>
    <xsl:when test="local-name()='Comment'">unpublished</xsl:when>
    <xsl:when test="local-name()='QAPage'">website</xsl:when>
    <xsl:when test="local-name()='BlogPosting'">blog</xsl:when>
    <xsl:when test="local-name()='DigitalDocument'">generic</xsl:when>
  </xsl:choose>
  <xsl:choose><xsl:when test="field[@name='author']">
      authors:<xsl:for-each select="field[@name='author']/summary[@name='name']">
          - name: "<xsl:value-of select="node()"/>"</xsl:for-each></xsl:when></xsl:choose><xsl:for-each select="summary">
      abstract: "<xsl:value-of select="node()"/>"</xsl:for-each><xsl:for-each select="field"><xsl:sort select="attribute::name"/><xsl:choose><xsl:when test="@name='headline'">
      title: "<xsl:value-of select="node()"/>"</xsl:when><xsl:when test="@name='publisher'"><xsl:for-each select="summary[@name='name']">
      publisher:
          - name: "<xsl:value-of select="node()"/>"</xsl:for-each></xsl:when><xsl:when test="@name='isbn'">
      isbn: "<xsl:value-of select="node()"/>"</xsl:when><xsl:when test="@name='numberOfPages'">
      pages: <xsl:value-of select="node()"/></xsl:when><xsl:when test="@name='license'">
      license: "<xsl:value-of select="node()"/>"</xsl:when><xsl:when test="@name='url'">
      url: "<xsl:value-of select="node()"/>"</xsl:when><xsl:when test="@name='genre'">
      keywords:
          - "<xsl:value-of select="node()"/>"</xsl:when><xsl:when test="@name='datePublished'">
      date-released: "<xsl:value-of select="node()"/>"</xsl:when>
    </xsl:choose></xsl:for-each>
<!--
    <xsl:choose><xsl:when test="field[@name='hasPart']">
      references:<xsl:for-each select="field[@name='hasPart']">
          - type: <xsl:choose>
    <xsl:when test="@type='CreativeWork'">generic</xsl:when>
    <xsl:when test="@type='Article'">article</xsl:when>
    <xsl:when test="@type='Report'">report</xsl:when>
    <xsl:when test="@type='Book'">book</xsl:when>
    <xsl:when test="@type='ScholarlyArticle'">article</xsl:when>
    <xsl:when test="@type='BlogPosting'">blog</xsl:when>
    <xsl:when test="@type='TechArticle'">article</xsl:when>
    <xsl:when test="@type='DigitalDocument'">data</xsl:when>
    <xsl:when test="@type='PublicationIssue'">article</xsl:when>
    <xsl:when test="@type='SoftwareApplication'">software-executable</xsl:when>
    <xsl:when test="@type='WebPage'">article</xsl:when>
    <xsl:when test="@type='WebSite'">website</xsl:when>
    <xsl:when test="@type='SoftwareSourceCode'">software-code</xsl:when>
    <xsl:when test="@type='Comment'">unpublished</xsl:when>
    <xsl:when test="@type='QAPage'">website</xsl:when>
    <xsl:when test="@type='BlogPosting'">blog</xsl:when>
    <xsl:when test="@type='DigitalDocument'">generic</xsl:when>
  </xsl:choose>
            authors: <xsl:value-of select="../field[@name='author']/summary[@name='name']"/>
            title: <xsl:for-each select="summary">"<xsl:value-of select="node()"/>"</xsl:for-each><xsl:for-each select="field[@name='url']">
            url: "<xsl:value-of select="node()"/>"</xsl:for-each></xsl:for-each></xsl:when></xsl:choose>--> </xsl:for-each>

  </xsl:template>
</xsl:stylesheet>
