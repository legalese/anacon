#!/usr/bin/env python

##
# AnaCon workflow script
# John J. Camilleri, Jan 2012
#
# Usage:
#	AnaCon.py Contract.txt
##

import sys
import re
import os
from subprocess import Popen, PIPE, STDOUT, call
from time import time, strftime
from cgi import escape

# Some regexs and functions used throughout
braceRegexGrouped   = r"\{\s*(.+?)\s*\}\s*(.+?)\s*\{\s*(.+?)\s*\}"
braceRegexUngrouped = r"\{.+?\}.+?\{.+?\}"
def unpadBraces(s):
	return s.replace("{ ", "{").replace(" }", "}").replace(" - ", "-").strip()
def unpadAll(s):
	return re.sub(r"\s*(\W)\s*", "\\1", s).strip()
def padAll(s):
	s = re.sub(r"([^\w\d\-\_])", " \\1 ", s).strip()
	s = re.sub(r"\s{2,}", " ", s).strip()
	return s

##
# Extract the CNL clauses from the contract
# @param filename
# @return List of CNL clauses
def extractClauses(filename):
	return extract(filename, "clauses")

##
# Extract the CL contradictions
# @param filename
# @return List of exclusive actions
def extractContradictions(filename):
	l = extract(filename, "contradictions")
	l = [re.sub("\s*#\s*", "#", i).strip() for i in l]
	return l

##
# Extract generic section from contract file
# @param filename
# @param section
# @return List of items
def extract(filename, section):
	try:
		with open(filename, 'r') as f:
			content = f.read()
		m = re.search("\["+section+"\](.*)\[/"+section+"\]", content, re.DOTALL)
		l = map(str.strip, m.group(1).strip().split(";"))
		while len(l)>0 and l[-1]=='':
			l = l[:-1]
		return l
	except IOError as (errno, strerror):
		exit("I/O error({0}): {1}".format(errno, strerror))

##
# Convert CNL clauses to CL
# @param listCNL List of CNL clauses
# @param startcat Start category
# @return List of CL clauses
def translateToCL(listCNL, startcat="Clause"):
	listCL = translate("ENG", startcat, listCNL, failOnError=True)
	listCL = [unpadBraces(i) for i in listCL]
	return listCL

##
# Convert CL clauses to CNL
# @param listCL List of CL clauses
# @param startcat Start category
# @return List of CNL clauses
def translateToCNL(listCL, startcat="Clause"):
	listCNL = translate("CLAN", startcat, listCL, failOnError=False)
	return listCNL

##
# Generic translate function (using GF)
# @param fromLang From language (ENG or CLAN)
# @param startcat Start category
# @param listIn List of clauses to be translated
# @keyparam failOnError Fail when translation fails? (boolean)
# @return List of translated clauses
def translate(fromLang, startcat, listIn, failOnError=False):
	listOut = []
	for index,line in enumerate(listIn):
		try:
			p = Popen(["runghc", "ClParser.hs", fromLang, startcat], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
		except OSError as (en,em):
			exit("Unable to execute runghc: %s"%em)
		out = p.communicate(input=line+"\n")[0]
		if p.wait() == 0:
			listOut.append(out)
		else:
			if failOnError:
				print "Cannot translate line %d: \"%s\" " % (index+1, line)
				exit(out.strip())
	return listOut

##
# Extract actions, rename them and write dictionary to file
# @param clClauses
# @param filename
# @return None
def obtainDictionary(clClauses, filename):
	# Find all actions
	acts = []
	for cx,c in enumerate(clClauses):
		acts += re.findall(braceRegexUngrouped, c)

	# Make unique, give names and put in dict
	dicto = {}
	acts = list(set(acts))
	for ax,a in enumerate(acts):
		key = "%s%d" % (chr(97+(ax/9)), (ax%9)+1)
		dicto[key] = unpadBraces(a)

	# Write to file
	with open(filename, 'w') as f:
		for key,value in dicto.items():
			f.write("%s = %s\n"%(key, value))

##
# Read dictionary into assoc array
# @param dictFilename Dictionary file
# @param tuples Extract terms as tuples?
# @return Dictionary
def readDictionary(dictFilename, tuples=False):
	d = {}
	with open(dictFilename, 'r') as f:
		for ix,line in enumerate(f):
			key = line.split('=')[0].strip()
			value = line.split('=')[1].strip()
			if tuples:
				value = re.search(braceRegexGrouped, value).groups()
			d[key] = value
	return d
	
##
# Replaces action names using the dictionary
# @param dictFilename Dictionary file
# @param clauses List of clauses to rename
# @param reverse Rename from alias back to full name?
# @param usepars Use parenthesis?
# @return Renamed clauses
def replaceActNames(dictFilename, clauses, reverse=False, usepars=False):
	dic = readDictionary(dictFilename)
	for key,value in dic.items():
		for cx,c in enumerate(clauses):
			if not(reverse):
				# We're translating input
				search = value
				replace = key
			else:
				# We're translating output
				search = key
				replace = "("+value+")" if usepars else value
			clauses[cx] = c.replace(search, replace)
	return clauses

##
# Check validity of contract
# @param dictFilename
# @param clContras
# @return Boolean
def checkContradictions(dictFilename, clContras):
	dic = readDictionary(dictFilename)
	for cx,contra in enumerate(clContras):
		for i in range(0,2):
			act = contra.split('#')[i].strip()
			if not(act in dic.values()):
				print "Error in contradictory actions: '%s' does not appear in clauses."%act
				return False
	return True

##
# Write contract as CLAN XML file
# @param xmlFilename
# @param clClauses
# @param clContras
def writeCLANXML(xmlFilename, clClauses, clContras):
	with open(xmlFilename, "w") as f:
		f.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" + "\n")
		f.write("<!-- Generated by " + sys.argv[0] + " on " + strftime("%Y-%m-%d %H:%M:%S") + " -->" + "\n")
		f.write("<contract>" + "\n")
		f.write("\t<clauses>" + "\n")
		for c in clClauses:
			f.write("\t\t<clause>" + unpadAll(escape(c)) + "</clause>" + "\n")
		f.write("\t</clauses>" + "\n")
		f.write("\t<concurrentActions>" + "\n")
		for c in clContras:
			f.write("\t\t<action>" + c + "</action>" + "\n")
		f.write("\t</concurrentActions>" + "\n")
		f.write("</contract>" + "\n");

##
# Invoke CLAN
# @param xmlFilename
# @param outFilename
# @return Exit value from call to CLAN
def analyseWithCLAN(xmlFilename, outFilename):
	return call(["java", "-jar", "clan-gui-1.0.0-clan.jar", xmlFilename, outFilename], stderr=PIPE) # output suppressed

##
# Parse CLAN output, linearizing to CNL via GF as necessary
# @param filename
def formatCLANOutput(filename):
	def p(s):
		print "    " + s.strip()
	with open(filename, 'r') as f:
		lines = f.readlines()
		ix = 2
		counterexamples = []
		while ix < len(lines):
			counterexamples.append( (lines[ix].strip(), lines[ix+1].strip()) )
			ix+=3

	print lines[0].strip() # Conflict or not
	if len(counterexamples) > 0:
		print "%d counter examples found (only showing first)" % len(counterexamples)
		
		for clause,trace in counterexamples:
		
			# Handle the clause
			print "Clause:"
			p(clause)

			# Handle the action trace
			print "Trace:"
			for ix,ac in enumerate(trace.split(',')):
				ac = ac.strip()
				ac = replaceActNames(dictFilename, [ac], reverse=True, usepars=False)[0]
				ac = padAll(ac)
				lin = translateToCNL([ac], startcat="Act")
				if lin:
					p(" %d. %s" % (ix+1, lin[0]))
				else:
					p("(%d) %s" % (ix+1, ac))
			
			break # only consider first counter-example

# ----------------------------------------------------------------------

if __name__ == "__main__":
	
	# Check usage & set file names
	if (len(sys.argv) < 2):
		exit("You must specify a contract filename, e.g.: " + sys.argv[0] + " contract.txt")
	filename = sys.argv[1]
	prefix = os.path.splitext(filename)[0]
	dictFilename = prefix+"_dictionary.txt"
	xmlFilename = prefix+"_in.xml"
	outFilename = prefix+"_out.txt"
	
	print "Converting to CL"
	nlClauses = extractClauses(filename)
	if len(nlClauses) == 0:
		exit("Contract contains no clauses.")
	clClauses = translateToCL(nlClauses)

	print "Extracting dictionary (%s)" % dictFilename
	obtainDictionary(clClauses, dictFilename)
	
	print "Checking contradictory actions"
	clContras = extractContradictions(filename)
	if not checkContradictions(dictFilename, clContras):
		exit("Invalid contract file.")

	print "Renaming actions"
	clClauses = replaceActNames(dictFilename, clClauses)
	clContras = replaceActNames(dictFilename, clContras)

	print "Writing CLAN input file (%s)" % xmlFilename
	writeCLANXML(xmlFilename, clClauses, clContras)

	print "Analysing with CLAN"
	t1 = time()
	if analyseWithCLAN(xmlFilename, outFilename) != 0:
		exit("Failed")
	t2 = time()
	print "Completed in %0.3fs" % ((t2-t1))
		
	print "CLAN Output (%s):" % outFilename,
	formatCLANOutput(outFilename)

	exit(0)
