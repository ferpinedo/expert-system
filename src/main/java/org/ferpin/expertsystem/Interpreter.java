package org.ferpin.expertsystem;

import org.apache.commons.lang3.text.WordUtils;


public class Interpreter {
    private Interpreter(){}

    private static MessageType messageType;

    public static MessageType getMessageType() {
        return messageType;
    }

    public static void setMessageType(MessageType messageType) {
        Interpreter.messageType = messageType;
    }

    public static String processMessage(String message){
        String answer = "";
        message = message.toLowerCase();
        switch (messageType) {
            case NAME:
                answer = "Muy bien, " + WordUtils.capitalize(getName(message)) + ", cuéntame ¿Por qué viniste a consulta? ¿Qué es lo que te pasa?";
                break;
            case SYMPTOM:
                // System.out.println("Asking symptom");
                answer = getSymptomAnswer(message);
                break;
            case YES_NO_ANSWER:
                // System.out.println("Asking yes no answer");
                answer = answer(message);
                break;
            case GOODBYE:
                answer = "De acuerdo, muchas gracias por venir, hasta pronto";
            default:
                answer = "No te entedí muy bien, puedes repetir eso último";
        }
        return answer;

    }

    private static String getName(String text) {
        messageType = MessageType.SYMPTOM;
        String[] words = text.split(" ");
        for (int i = 0; i < words.length; i++) {
            if (words[i].equals("me"))
                return words[i + 2];

            if (words[i].equals("nombre"))
                return words[i + 2];
        }
        return text;
    }

    private static String getSymptomAnswer(String text) {
        text = deleteWords(text, "mucho ", "se ", "los ", "las ", ",",
            "ellos ", "ellas ", "ello ", "ella ", "parte ", "superior ", "inferior ");

        // System.out.println("Processing " + text);
        String[] words = text.split(" ");
        if (words[0].equals("no")) {
            messageType = MessageType.YES_NO_ANSWER;
            return ask();
        }

        StringBuilder symptom = new StringBuilder();
        for (int i = 0; i < words.length; i++) {
            if (words[i].equals("tengo") || words[i].equals("siento") ||
                words[i].equals("si") || words[i].equals("sí")) {
                if (i + 1 < words.length - 1) {
                    symptom = new StringBuilder(WordUtils.capitalize(words[i + 1]));
                    for (int j = i + 2; j < words.length; j++) {
                        symptom.append(" ").append(words[j]);
                    }
                } else {
                    symptom = new StringBuilder(WordUtils.capitalize(words[i + 1]));
                }
                // System.out.println(symptom);
                addSymptom(symptom.toString());
                return symptom + ". Entendido. ¿Algo más?";
            }
        }
        addSymptom(text);
        return WordUtils.capitalize(text) + ". Entendido. ¿Algo más?";
    }

    private static void addSymptom(String text) {
//        // System.out.println(PrologPuppeteer.run("processEvidence", "["+ getSymptom(text) + "]") ?
//            "Evidencia encontrada":"Evidencia no encontrada");
        String query = "processEvidence([" + getSymptom(text) + "])";
        // System.out.println(query);
        PrologPuppeteer.simpleQuery2(query);
    }

    private static String getSymptom(String symptom) {
        symptom = symptom.toLowerCase();
        symptom = deleteWords(symptom, "en ", "el ", "la ", "de ");
        String[] words = symptom.split(" ");
        symptom = "";
        for (int i = 0; i < words.length; i++) {
            if (!(i == words.length - 1)) {
                symptom = symptom + words[i] + "_";
            } else {
                symptom = symptom + words[i];
            }
        }
        // System.out.println("Prolog symptom: " + symptom);
        return symptom;
    }

    private static String deleteWords(String text, String ...words) {
        for (String wordToDelete: words) {
            text = text.replaceAll(wordToDelete, "");
        }
        return text;
    }

    public static String ask() {

        try {
            String rule = PrologPuppeteer.query("searchForTrueRule").get(0);
            String conclusion = PrologPuppeteer.query("conclusion", rule).get(0);
            conclusion = conclusion.replaceAll("_", " ");
            messageType = MessageType.GOODBYE;
            return "Lo he detectado, tú tienes la enfermedad llamada: " + conclusion;
        } catch (IndexOutOfBoundsException e) {
            try {
                String toAsk = PrologPuppeteer.query("ask").get(0);
                toAsk = toAsk.replaceAll("_", " ");
                return "¿Has sentido " + toAsk +"?";
            } catch (IndexOutOfBoundsException exc) {
                messageType = MessageType.GOODBYE;
                return "Increíble... Me temo que esa enfermedad es desconocida para mí."
                    + "Lo investigaré y te aviso";
            }


        }

    }

    public static String answer(String text) {
        text = deleteWords(text, ",");
        String[] words = text.split(" ");
        if (words[0].equals("si") || words[0].equals("sí")) {
            String query = "answer(yes)";
            // System.out.println(query);
            PrologPuppeteer.simpleQuery2(query);
        } else {
            String query = "answer(no)";
            // System.out.println(query);
            PrologPuppeteer.simpleQuery2(query);
        }
        return ask();
    }

    private static boolean isNumeric(String str) {
        try {
            Double.parseDouble(str);
            return true;
        } catch(NumberFormatException e){
            return false;
        }
    }

}
